module ErgoDex.Amm.Orders where

import           Data.Tuple.Extra
import           Data.Bifunctor
import           Control.Monad    (when)
import           Data.Function
import           Data.Aeson       (FromJSON(..), ToJSON(..), object, (.=), (.:))
import qualified Data.Aeson       as JSON
import           GHC.Generics     (Generic)

import           Ledger
import           PlutusTx.IsData.Class
import qualified Plutus.V1.Ledger.Value as V
import           Ledger.Value           (AssetClass(..), assetClassValueOf, flattenValue)
import qualified Ledger.Ada             as Ada
import           PlutusTx.Prelude       (divide)

import CardanoTx.Models
import ErgoDex.Types
import ErgoDex.Class
import ErgoDex.State
import ErgoDex.Amm.Pool                (PoolId(..))
import ErgoDex.Contracts.Types
import ErgoDex.Contracts.Proxy.Swap
import ErgoDex.Contracts.Proxy.Deposit
import ErgoDex.Contracts.Proxy.Redeem
import GHC.RTS.Flags (TraceFlags(timestamp))

data Swap = Swap
  { swapPoolId      :: PoolId
  , swapBaseIn      :: Amount Base
  , swapMinQuoteOut :: Amount Quote
  , swapBase        :: Coin Base
  , swapQuote       :: Coin Quote
  , swapExFee       :: ExFeePerToken
  , swapRewardPkh   :: PubKeyHash
  , swapRewardSPkh  :: Maybe StakePubKeyHash
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromLedger Swap where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(KnownDatum (Datum d)), ..} =
    case fromBuiltinData d of
      (Just SwapConfig{..}) -> do
        let
          swapBase = Coin base
          baseIn   = Amount $ assetClassValueOf fullTxOutValue base
          minBase  =
            if isAda swapBase
              then baseAmount + divide (minQuoteAmount * exFeePerTokenNum) exFeePerTokenDen
              else baseAmount
        when (unAmount baseIn < minBase) Nothing
        Just $ OnChain fout Swap
          { swapPoolId      = PoolId $ Coin poolNft
          , swapBaseIn      = Amount baseAmount
          , swapMinQuoteOut = Amount minQuoteAmount
          , swapBase        = swapBase
          , swapQuote       = Coin quote
          , swapExFee       = ExFeePerToken exFeePerTokenNum exFeePerTokenDen
          , swapRewardPkh   = rewardPkh
          , swapRewardSPkh  = fmap StakePubKeyHash stakePkh
          }
      _ -> Nothing
  parseFromLedger _ = Nothing

data Deposit = Deposit
  { depositPoolId     :: PoolId
  , depositPair       :: (AssetEntry, AssetEntry)
  , depositExFee      :: ExFee
  , depositRewardPkh  :: PubKeyHash
  , depositRewardSPkh :: Maybe StakePubKeyHash
  , adaCollateral     :: Amount Lovelace
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromLedger Deposit where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(KnownDatum (Datum d)), ..} =
    case fromBuiltinData d of
      (Just DepositConfig{..}) -> do
        let adaIn       = Ada.getLovelace $ Ada.fromValue fullTxOutValue
            adaDeclared = exFee + collateralAda
        when (adaIn < adaDeclared) Nothing
        case extractPairValue fullTxOutValue of
          [assetX, assetY] ->
              Just $ OnChain fout Deposit
                { depositPoolId     = PoolId $ Coin poolNft
                , depositPair       = pair
                , depositExFee      = ExFee $ Amount exFee
                , depositRewardPkh  = rewardPkh
                , depositRewardSPkh = fmap StakePubKeyHash stakePkh
                , adaCollateral     = Amount collateralAda
                }
            where
              toEntry = uncurry3 assetEntry
              pair    = bimap toEntry toEntry (assetX, assetY)
          _ -> Nothing
      _ -> Nothing
  parseFromLedger _ = Nothing

data Redeem = Redeem
  { redeemPoolId     :: PoolId
  , redeemLqIn       :: Amount Liquidity
  , redeemLq         :: Coin Liquidity
  , redeemExFee      :: ExFee
  , redeemRewardPkh  :: PubKeyHash
  , redeemRewardSPkh :: Maybe StakePubKeyHash
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromLedger Redeem where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(KnownDatum (Datum d)), ..} =
    case fromBuiltinData d of
      (Just cfg@RedeemConfig{..}) -> do
        let adaIn = Ada.getLovelace $ Ada.fromValue fullTxOutValue
        case extractLpToken fullTxOutValue cfg of
          Just (ac, tn, v) ->
              Just $ OnChain fout Redeem
                { redeemPoolId    = PoolId $ Coin poolNft
                , redeemLqIn      = Amount v
                , redeemLq        = Coin $ AssetClass (ac, tn)
                , redeemExFee     = ExFee $ Amount exFee
                , redeemRewardPkh = rewardPkh
                , redeemRewardSPkh = fmap StakePubKeyHash stakePkh
                }
          _ -> Nothing
      _ -> Nothing
  parseFromLedger _ = Nothing

extractLpToken :: Value -> RedeemConfig -> Maybe (CurrencySymbol, TokenName, Integer)
extractLpToken inputValue RedeemConfig{..} =
    if (lpValue <= 0) then Nothing else Just (lpCs, lpTn, lpValue)
  where
    (lpCs, lpTn) = unAssetClass poolLp
    lpValue = V.assetClassValueOf inputValue poolLp

-- Extract pair of assets for order (Deposit|Redeem) from a given value.
extractPairValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
extractPairValue inputValue =
    if length flattenedInput == 2
    then flattenedInput
    else filter (\(s, _, _) -> s /= Ada.adaSymbol) flattenedInput
  where
    flattenedInput = flattenValue inputValue

data OrderAction a where
  SwapAction    :: Swap    -> OrderAction Swap
  DepositAction :: Deposit -> OrderAction Deposit
  RedeemAction  :: Redeem  -> OrderAction Redeem

instance Show (OrderAction a) where
  show (SwapAction swap)       = show swap
  show (DepositAction deposit) = show deposit
  show (RedeemAction redeem)   = show redeem

instance Eq (OrderAction a) where
  (SwapAction x) == (SwapAction y)       = x == y
  (DepositAction x) == (DepositAction y) = x == y
  (RedeemAction x) == (RedeemAction y)   = x == y

data Order a = Order
  { orderPoolId :: PoolId
  , orderAction :: OrderAction a
  } deriving (Show, Eq)

data AnyOrder = forall a . AnyOrder
  { anyOrderPoolId    :: PoolId
  , anyOrderTimestamp :: Integer
  , anyOrderAction    :: OrderAction a
  }

data ActionKind = SwapK | DepositK | RedeemK
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToJSON AnyOrder where
  toJSON (AnyOrder poolId timestamp (SwapAction swap)) =
    object [ "kind"      .= SwapK
           , "poolId"    .= poolId
           , "action"    .= toJSON swap
           , "timestamp" .= timestamp
           ]
  toJSON (AnyOrder poolId timestamp (DepositAction deposit)) =
    object [ "kind"   .= DepositK
           , "poolId" .= poolId
           , "action" .= toJSON deposit
           , "timestamp" .= timestamp
           ]
  toJSON (AnyOrder poolId timestamp (RedeemAction redeem)) =
    object [ "kind"   .= RedeemK
           , "poolId" .= poolId
           , "action" .= toJSON redeem
           , "timestamp" .= timestamp
           ]

instance FromJSON AnyOrder where
  parseJSON (JSON.Object v) = do
    kind   <- v .: "kind"
    poolId <- v .: "poolId"
    timestamp <- v.: "timestamp"
    let actionP = v .: "action"

    case kind of
      SwapK    -> actionP >>= JSON.parseJSON & fmap (AnyOrder poolId timestamp . SwapAction)
      DepositK -> actionP >>= JSON.parseJSON & fmap (AnyOrder poolId timestamp . DepositAction)
      RedeemK  -> actionP >>= JSON.parseJSON & fmap (AnyOrder poolId timestamp . RedeemAction)

  parseJSON _ = fail "expected an object"
