{-# LANGUAGE RecordWildCards #-}

module Explorer.Models where

import           Data.Aeson        (FromJSON, Value)
import           Data.Aeson.Types
import qualified Data.Either.Combinators as EC
import           Data.Maybe
import           Data.String       (IsString(..))
import qualified Data.Text         as T
import           GHC.Generics

import qualified Ledger                     as P
import qualified Cardano.Api                as CA
import qualified Plutus.V1.Ledger.Value     as Value
import qualified Cardano.Api.Shelley        as CAS
import           Explorer.Types
import           Explorer.Class
import qualified CardanoTx.Models       as Tx
import           CardanoTx.Value
import qualified CardanoTx.Models           as Tx
import qualified PlutusTx.Builtins.Internal as BI

import qualified Data.Set        as Set
import qualified Data.ByteString as DBS

import qualified Cardano.Api as Api
import           Cardano.Api.Shelley   (ProtocolParameters(..), PoolId)
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Summary as HSummary
import           Ouroboros.Consensus.HardFork.History.EraParams as EP
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Counting
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex

--todo: to utils
mkByteString :: T.Text -> DBS.ByteString
mkByteString input = unsafeFromEither input (Hex.decode . T.encodeUtf8 $ input)

--todo: to utils
unsafeFromEither :: T.Text -> Either String a -> a
unsafeFromEither input (Left err)    = Prelude.error ((show err) ++ (show input))
unsafeFromEither _ (Right value) = value

data SystemEnv = SystemEnv
  { pparams'           :: ProtocolParameters
  , network'           :: Api.NetworkId
  , sysstart'          :: SystemStart
  , pools'             :: Set.Set PoolId
  , eraHistory'        :: Api.EraHistory Api.CardanoMode
  , collateralPercent' :: Int
  } deriving (Show, Generic)

instance Show (Api.EraHistory Api.CardanoMode) where
  show _ = "Era history"

instance FromJSON SystemEnv where
  parseJSON = withObject "SystemEnv" $ \o -> do
    pparams'           <- o .: "pparams"
    sysstart'          <- o .: "sysstart"
    collateralPercent' <- o .: "collateralPercent"
    return
      SystemEnv
        { pparams'           = pparams'
        , network'           = Api.Testnet $ Api.NetworkMagic 1097911063
        , sysstart'          = sysstart'
        , pools'             = Set.empty
        , eraHistory'        = dummyEraHistory
        , collateralPercent' = collateralPercent'
        }

dummyEraHistory :: Api.EraHistory Api.CardanoMode
dummyEraHistory =
  Api.EraHistory
    Api.CardanoMode
    (History.mkInterpreter $ Summary $ NonEmptyOne $
      EraSummary {
          eraStart  = initBound
        , eraEnd    = EraUnbounded
        , eraParams = EraParams {
            eraEpochSize  = EpochSize 100
          , eraSlotLength = mkSlotLength 100
          , eraSafeZone   = UnsafeIndefiniteSafeZone
          }
        }
    )

data Paging = Paging
  { offset :: Int
  , limit  :: Int
  } deriving (Show, Generic, FromJSON)

data Items a = Items
  { items :: [a]
  , total :: Int
  } deriving (Show, Generic, FromJSON)

data FullTxOut = FullTxOut
  { ref         :: OutRef
  , txHash      :: TxHash
  , index       :: Int
  , globalIndex :: Gix
  , addr        :: Addr
  , rawAddr     :: RawAddr
  , value       :: [OutAsset]
  , dataHash    :: Maybe P.DatumHash
  , data'       :: Maybe P.Datum
  } deriving (Show, Generic)

instance FromJSON FullTxOut where
  parseJSON = withObject "quickblueFullTxOut" $ \o -> do
    ref          <- OutRef <$> o .: "ref"
    txHash       <- TxHash <$> o .: "txHash"
    index        <- o .: "index"
    globalIndex  <- Gix <$> o .: "globalIndex"
    addr         <- Addr <$> o .: "addr"
    rawAddr      <- RawAddr <$> o .: "rawAddr"
    value        <- o .: "value"
    dataHash     <- o .:? "dataHash"
    rawDataM     <- (o .:? "data")
    let
      dataTestE  = rawDataM
      dataTest0  = dataTestE >>= (\m -> EC.rightToMaybe $ CA.scriptDataFromJson CA.ScriptDataJsonDetailedSchema m)
      dataTest1  = fmap CAS.toPlutusData dataTest0
      dataTest2  = fmap BI.dataToBuiltinData dataTest1
      dataEither = fmap (\d -> P.Datum d) dataTest2
      data'      = dataEither
    return FullTxOut{..}

instance ToCardanoTx FullTxOut Tx.FullTxOut where
  toCardanoTx FullTxOut{..} = Tx.FullTxOut
    { fullTxOutRef       = toCardanoTx ref
    , fullTxOutAddress   = toCardanoTx addr
    , fullTxOutValue     = foldr (\a acc -> unionVal acc (toCardanoTx a)) mempty value
    , fullTxOutDatumHash = dataHash
    , fullTxOutDatum     = data'
    }

data OutAsset = OutAsset
  { policy      :: PolicyId
  , name        :: AssetName
  , quantity    :: Integer
  } deriving (Show, Generic)

instance FromJSON OutAsset where
  parseJSON = withObject "quickblueOutAsset" $ \o -> do
    policy      <- PolicyId <$> o .: "policyId"
    name        <- AssetName <$> o .: "name"
    quantity    <- o .: "quantity"
    return OutAsset{..}

instance ToCardanoTx OutAsset P.Value where
  toCardanoTx OutAsset{..} = Value.singleton p n quantity
    where
      p = Value.CurrencySymbol $ BI.BuiltinByteString $ mkByteString $ unPolicyId policy
      n = fromString $ T.unpack $ unAssetName name
