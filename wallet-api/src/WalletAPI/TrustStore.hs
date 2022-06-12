module WalletAPI.TrustStore
  ( SecretFile(..)
  , KeyPass(..)
  , KeyLookupError(..)
  , TsInitializationError(..)
  , TsImportError(..)
  , TrustStore(..)
  , mkTrustStore
  , mkTrustStoreUnsafe
  , importTrustStoreFromCardano
  ) where

import           RIO
import qualified Dhall                  as D
import qualified Data.ByteString.Base16  as Hex
import           Data.Aeson
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteArray         as BA
import qualified Data.Text.Encoding     as T
import           Data.Aeson

import qualified Crypto.Hash         as H
import           Crypto.Cipher.AES   (AES256)
import           Crypto.Cipher.Types (makeIV)
import           Crypto.Random.Types
import qualified Cardano.Api         as Crypto

import WalletAPI.Internal.Crypto
import WalletAPI.Internal.Models (SecretEnvelope(..), TrustStoreFile(..))
import Cardano.Api.Byron         (AsType)
import Cardano.Api.Shelley       (SerialiseAsRawBytes)

newtype SecretFile = SecretFile { unSigningKeyFile :: FilePath } deriving Generic

instance D.FromDhall SecretFile

newtype KeyPass = KeyPass { unKeyPass :: Text } deriving Generic

instance D.FromDhall KeyPass

data KeyLookupError
  = DecryptionFailed
  | NotInitialized
  | StoreFileCorrupted
  deriving (Show, Exception)

data TsInitializationError
  = InitializationError
  | AlreadyInitialized
  deriving (Show, Exception)

data TsImportError
  = WrongEnvelopeType
  | EnvelopeFileInaccessible
  | EnvelopeFileCorrupted
  deriving (Show, Exception)

data TrustStore f krole = TrustStore
  { init          :: KeyPass -> f ()
  , readSK        :: KeyPass -> f (Crypto.SigningKey krole)
  , readVK        :: f (Crypto.VerificationKey krole)
  , isInitialized :: f Bool
  }

mkTrustStore
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SerialiseAsRawBytes (Crypto.VerificationKey krole)
  => SerialiseAsRawBytes (Crypto.SigningKey krole)
  => Crypto.Key krole
  => AsType krole
  -> SecretFile
  -> TrustStore f krole
mkTrustStore krole file = TrustStore
  { init          = init' krole file
  , readSK        = readSK' krole file
  , readVK        = readVK' krole file
  , isInitialized = isInitialized' file
  }

-- Mk TrustStore in Cardano format.
-- Note: In this format secrets are stored unencrypted.
mkTrustStoreUnsafe
  :: (MonadIO f, MonadThrow f, MonadThrow m)
  => Crypto.Key krole
  => AsType krole
  -> FilePath
  -> f (TrustStore m krole)
mkTrustStoreUnsafe krole srcFile = do
  sk <- absorbEnvelopeError =<< liftIO (Crypto.readFileTextEnvelope (Crypto.AsSigningKey krole) srcFile)
  let vk = Crypto.getVerificationKey sk
  pure $ TrustStore
    { init          = const $ throwM AlreadyInitialized
    , readSK        = const $ pure sk
    , readVK        = pure vk
    , isInitialized = pure True
    }

-- Instantiate TrustStore by importing key from Cardano node .skey file.
importTrustStoreFromCardano
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SerialiseAsRawBytes (Crypto.VerificationKey krole)
  => SerialiseAsRawBytes (Crypto.SigningKey krole)
  => Crypto.Key krole
  => AsType krole
  -> SecretFile
  -> FilePath
  -> KeyPass
  -> f (TrustStore f krole)
importTrustStoreFromCardano krole targetFile srcFile pass = do
  sk <- absorbEnvelopeError =<< liftIO (Crypto.readFileTextEnvelope (Crypto.AsSigningKey krole) srcFile)
  let vkEncoded = EncodedVK $ Crypto.serialiseToRawBytes $ Crypto.getVerificationKey sk
  envelope <- encryptKey sk pass
  writeTS targetFile $ TrustStoreFile envelope vkEncoded
  pure $ mkTrustStore krole targetFile

init'
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SerialiseAsRawBytes (Crypto.VerificationKey krole)
  => SerialiseAsRawBytes (Crypto.SigningKey krole)
  => Crypto.Key krole
  => AsType krole
  -> SecretFile
  -> KeyPass
  -> f ()
init' krole file pass = do
  sk <- liftIO $ Crypto.generateSigningKey krole
  let vkEncoded = EncodedVK $ Crypto.serialiseToRawBytes $ Crypto.getVerificationKey sk
  envelope <- encryptKey sk pass
  writeTS file $ TrustStoreFile envelope vkEncoded

isInitialized'
  :: MonadIO f
  => SecretFile
  -> f Bool
isInitialized' file = readTS file <&> isJust

readSK'
  :: (MonadIO f, MonadThrow f)
  => SerialiseAsRawBytes (Crypto.SigningKey krole)
  => AsType krole
  -> SecretFile
  -> KeyPass
  -> f (Crypto.SigningKey krole)
readSK' krole file pass = do
  TrustStoreFile{..} <- readTS file >>= maybe (throwM NotInitialized) pure
  maybe (throwM DecryptionFailed) pure $ decryptKey krole trustStoreSecret pass

readVK'
  :: (MonadIO f, MonadThrow f)
  => SerialiseAsRawBytes (Crypto.VerificationKey krole)
  => AsType krole
  -> SecretFile
  -> f (Crypto.VerificationKey krole)
readVK' krole file = do
  TrustStoreFile{trustStoreVK=EncodedVK rawVK} <- readTS file >>= maybe (throwM NotInitialized) pure
  maybe (throwM StoreFileCorrupted) pure $ Crypto.deserialiseFromRawBytes asVK rawVK
    where asVK = Crypto.AsVerificationKey krole

decryptKey
  :: SerialiseAsRawBytes (Crypto.SigningKey krole)
  => AsType krole
  -> SecretEnvelope
  -> KeyPass
  -> Maybe (Crypto.SigningKey krole)
decryptKey krole SecretEnvelope{secretCiphertext=Ciphertext text, secretSalt=salt, secretIv=EncodedIV rawIV} pass = do
  iv <- makeIV rawIV
  let encryptionKey = mkEncryptionKey pass salt
  rawSK <- either (const Nothing) Just $ decrypt encryptionKey iv text

  Crypto.deserialiseFromRawBytes asSK rawSK
    where asSK = Crypto.AsSigningKey krole

encryptKey
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SerialiseAsRawBytes (Crypto.SigningKey krole)
  => Crypto.SigningKey krole
  -> KeyPass
  -> f SecretEnvelope
encryptKey sk pass = do
  let saltLen = 16
  salt <- genRandomSalt saltLen
  iv   <- genRandomIV @_ @AES256 >>= maybe (throwM InitializationError) pure

  let
    iv'           = EncodedIV $ BS.pack $ BA.unpack iv
    encryptionKey = mkEncryptionKey pass salt
    rawSk         = Crypto.serialiseToRawBytes sk

  ciphertext <- either (const $ throwM InitializationError) pure $ encrypt encryptionKey iv rawSk <&> Ciphertext
  pure $ SecretEnvelope ciphertext salt iv'

mkEncryptionKey :: KeyPass -> Salt -> Key AES256 ByteString
mkEncryptionKey (KeyPass pass) (Salt salt) =
  Key $ BS.pack $ BA.unpack $ H.hashWith H.SHA256 $ T.encodeUtf8 pass <> salt

writeTS :: MonadIO f => SecretFile -> TrustStoreFile -> f ()
writeTS (SecretFile path) envelope =
  liftIO $ BL.writeFile path (encode envelope)

readTS :: MonadIO f => SecretFile -> f (Maybe TrustStoreFile)
readTS (SecretFile path) =
  liftIO $ BL.readFile path <&> decode

adaptEnvelopeError :: Crypto.FileError Crypto.TextEnvelopeError -> TsImportError
adaptEnvelopeError (Crypto.FileError _ (Crypto.TextEnvelopeTypeError _ _)) = WrongEnvelopeType
adaptEnvelopeError (Crypto.FileIOError _ _)                                = EnvelopeFileInaccessible
adaptEnvelopeError _                                                       = EnvelopeFileCorrupted

absorbEnvelopeError :: MonadThrow f => Either (Crypto.FileError Crypto.TextEnvelopeError) a -> f a
absorbEnvelopeError (Left err) = throwM $ adaptEnvelopeError err
absorbEnvelopeError (Right vl) = pure vl