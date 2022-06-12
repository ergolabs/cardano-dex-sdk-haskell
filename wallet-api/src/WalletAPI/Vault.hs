module WalletAPI.Vault where

import RIO

import qualified PlutusTx.Prelude    as PlutusTx
import           Ledger              (PubKeyHash(..))
import qualified Data.ByteString.Base16  as Hex
import           Data.Aeson
import qualified Data.Text.Encoding      as T
import qualified Cardano.Api         as Crypto
import qualified Cardano.Api         as C
import           Cardano.Api.Shelley
import           Algebra.Natural

import WalletAPI.TrustStore (TrustStore(TrustStore, readSK, readVK), KeyPass)

data VaultError = VaultCorrupted
  deriving (Show, Exception)

data Vault f = Vault
  { getSigningKey     :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , getPaymentKeyHash :: f (Hash PaymentKey) -- todo: dont mix Cardano.Api with Ledger.Api
  }

instance FunctorK Vault where
  fmapK xa alg =
    Vault
      { getSigningKey      = xa . getSigningKey alg
      , getPaymentKeyHash  = xa $ getPaymentKeyHash alg
      }

mkVault :: MonadThrow f => TrustStore f PaymentKey -> KeyPass -> Vault f
mkVault tstore pass = do
  Vault
    { getSigningKey     = getSigningKey' tstore pass
    , getPaymentKeyHash = getPaymentKeyHash' tstore
    }

getSigningKey' :: MonadThrow f => TrustStore f PaymentKey -> KeyPass -> PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
getSigningKey' TrustStore{readSK} pass pkh = do
  sk <- readSK pass <&> WitnessPaymentKey
  let vk   = extractVK (toShelleySigningKey sk)
      pkh' = PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes $ C.verificationKeyHash vk
  unless (pkh == pkh') (throwM VaultCorrupted)
  pure sk <&> Just

extractVK :: ShelleySigningKey -> VerificationKey PaymentKey
extractVK (ShelleyNormalSigningKey sk) =
  getVerificationKey
    . PaymentSigningKey
    $ sk
extractVK (ShelleyExtendedSigningKey sk) =
  (castVerificationKey :: VerificationKey PaymentExtendedKey -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk 

getPaymentKeyHash' :: Functor f => TrustStore f PaymentKey -> f (Hash PaymentKey)
getPaymentKeyHash' TrustStore{readVK} = readVK <&> C.verificationKeyHash
