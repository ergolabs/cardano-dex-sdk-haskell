module Explorer.Types where

import Data.Aeson   (FromJSON, ToJSON)
import Data.Text    hiding (words, head)
import Data.String  (IsString(..))
import Text.Read (readMaybe)
import GHC.Generics

import qualified Ledger as P
import           Explorer.Class
import qualified CardanoTx.Models  as Core
import qualified CardanoTx.Address as Core

-- Bech32 encoded and rendered address
newtype Addr = Addr { unAddr :: Text }
  deriving (Generic, FromJSON)
  deriving newtype (Show)

newtype RawAddr = RawAddr { unRawAddr :: Text }
    deriving (Generic, FromJSON)
    deriving newtype (Show)

instance ToCardanoTx Addr P.Address where
  toCardanoTx (Addr addr) = maybe (error "Impossible") id (Core.readShellyAddress addr)

-- Payment credential encoded as a hex string
newtype PaymentCred = PaymentCred { unPaymentCred :: Text }
  deriving (Generic, FromJSON)
  deriving newtype (Show)

-- Output reference string composed of TxHash and OutIndex : {TxHash}#{OutIndex}
newtype OutRef = OutRef { unOutRef :: Text }
  deriving (Show, Generic, FromJSON)

instance ToCardanoTx OutRef P.TxOutRef where
  toCardanoTx (OutRef ref) = P.TxOutRef (fromString $ unpack hash) (read $ unpack index)
    where [hash, index] = splitOn ":" ref

-- A hex-encoded hash of minting policy
newtype PolicyId = PolicyId { unPolicyId :: Text }
  deriving (Generic, FromJSON)
  deriving newtype (Show)

-- Asset name represented as a utf-8 string
newtype AssetName = AssetName { unAssetName :: Text }
  deriving (Generic, FromJSON)
  deriving newtype (Show)

-- 32 bytes hash represented as a hex string
newtype Hash32 = Hash32 { unHash32 :: Text }
  deriving (Generic, FromJSON)
  deriving newtype (Show)

-- TX hash32 represented as a hex string
newtype TxHash = TxHash { unTxHash :: Text }
  deriving (Generic, FromJSON)
  deriving newtype (Show)

-- A global index
newtype Gix = Gix { unGix :: Integer }
  deriving (Generic, ToJSON, FromJSON)
  deriving newtype (Show)

newtype Limit = Limit { unLimit :: Integer }
  deriving (Generic, FromJSON)
  deriving newtype (Show)
