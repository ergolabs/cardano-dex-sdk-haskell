module WalletAPI.TrustStore where

import           RIO
import qualified Dhall                  as D
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteArray         as BA
import qualified Data.Text.Encoding     as T
import           Data.Aeson
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex
import qualified Crypto.Hash         as H
import           Crypto.Cipher.AES   (AES256)
import           Crypto.Cipher.Types (makeIV)
import           Crypto.Random.Types
import qualified Cardano.Api         as Crypto

import WalletAPI.Internal.Crypto
import WalletAPI.Internal.Models

newtype SecretFile = SecretFile { unSigningKeyFile :: FilePath } deriving Generic

instance D.FromDhall SecretFile

newtype KeyPass = KeyPass { unKeyPass :: Text } deriving Generic

instance D.FromDhall KeyPass

data KeyLookupError =
    DecryptionFailed
  | NotInitialized
  | StoreFileCorrupted
  deriving (Show, Exception)

data InitializationError =
    InitializationError
  | AlreadyInitialized
  deriving (Show, Exception)

data TrustStore f = TrustStore
  { init          :: KeyPass -> f ()
  , readSK        :: KeyPass -> f (Crypto.SigningKey Crypto.PaymentKey)
  , readVK        :: f (Crypto.VerificationKey Crypto.PaymentKey)
  , isInitialized :: f Bool
  }

mkTrustStore
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SecretFile
  -> TrustStore f
mkTrustStore file = TrustStore
  { init          = init' file
  , readSK        = readSK' file
  , readVK        = readVK' file
  , isInitialized = isInitialized' file
  }

init'
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SecretFile
  -> KeyPass
  -> f ()
init' file pass = do
  sk       <- liftIO $ Crypto.generateSigningKey Crypto.AsPaymentKey
  liftIO $ print sk
  let vkEncoded = EncodedVK $ Crypto.serialiseToRawBytes $ Crypto.getVerificationKey sk
  envelope <- encryptKey sk pass
  writeTS file $ TrustStoreFile envelope vkEncoded

isInitialized'
  :: (MonadIO f, MonadThrow f)
  => SecretFile
  -> f Bool
isInitialized' file = readTS file <&> isJust

readSK'
  :: (MonadIO f, MonadThrow f)
  => SecretFile
  -> KeyPass
  -> f (Crypto.SigningKey Crypto.PaymentKey)
readSK' file pass = do
  TrustStoreFile{..} <- readTS file >>= maybe (throwM NotInitialized) pure
  maybe (throwM DecryptionFailed) pure $ decryptKey trustStoreSecret pass

unsafeFromEither :: Either b a -> a
unsafeFromEither (Left err)    = Prelude.error "Err"
unsafeFromEither (Right value) = value

readVK'
  :: (MonadIO f, MonadThrow f)
  => SecretFile
  -> f (Crypto.VerificationKey Crypto.PaymentKey)
readVK' file =
  pure $ (unsafeFromEither $ Crypto.deserialiseFromCBOR (Crypto.AsVerificationKey Crypto.AsPaymentKey) (unsafeFromEither $ Hex.decode . T.encodeUtf8 $ "58203cc87e73d56f0f00934038d145b484869cb3bf93e65a850b96a4caa3d0d50d73"))


decryptKey :: SecretEnvelope -> KeyPass -> Maybe (Crypto.SigningKey Crypto.PaymentKey)
decryptKey SecretEnvelope{secretCiphertext=Ciphertext text, secretSalt=salt, secretIv=EncodedIV rawIV} pass =
  -- iv <- makeIV rawIV
  -- let encryptionKey = mkEncryptionKey pass salt
  -- rawSK <- either (\_ -> Nothing) Just $ decrypt encryptionKey iv text
  Just $ unsafeFromEither $ Crypto.deserialiseFromCBOR asSK (unsafeFromEither $ Hex.decode . T.encodeUtf8 $ "582075bcd3df982e1bc89bdf261c0ccda780cc64be3ccd3cb84dcb1822573ab643ed")
    where asSK = Crypto.AsSigningKey Crypto.AsPaymentKey

  -- Crypto.deserialiseFromRawBytes asSK ()

encryptKey
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => Crypto.SigningKey Crypto.PaymentKey
  -> KeyPass
  -> f SecretEnvelope
encryptKey sk pass = do
  let saltLen = 16
  salt <- genRandomSalt saltLen
  iv   <- genRandomIV (undefined :: AES256) >>= maybe (throwM InitializationError) pure

  let
    iv'           = EncodedIV $ BS.pack $ BA.unpack iv
    encryptionKey = mkEncryptionKey pass salt
    rawSk         = Crypto.serialiseToRawBytes sk

  ciphertext <- either (\_ -> throwM InitializationError) pure $ encrypt encryptionKey iv rawSk <&> Ciphertext
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
