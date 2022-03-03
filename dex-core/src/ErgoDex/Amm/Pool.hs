module ErgoDex.Amm.Pool where

import Data.Bifunctor
import Data.Functor
import Data.Aeson     (FromJSON, ToJSON)
import GHC.Generics   (Generic)
import           RIO

import Ledger
import Ledger.Value                    (assetClassValue, assetClassValueOf)
import Ledger.Typed.Scripts.Validators
import PlutusTx.IsData.Class
import PlutusTx.Sqrt
import PlutusTx.Numeric                (AdditiveMonoid(zero))
import Plutus.V1.Ledger.Ada            (lovelaceValueOf)

import           CardanoTx.Models
import           ErgoDex.Class
import           ErgoDex.Types
import           ErgoDex.State
import qualified ErgoDex.Contracts.Typed       as S
import           ErgoDex.Contracts.Types
import qualified ErgoDex.Contracts.Proxy.Order as W
import           ErgoDex.Contracts.Pool
import           ErgoDex.Contracts.OffChain
import           ErgoDex.Amm.Constants         (minSafeOutputAmount)

newtype PoolId = PoolId { unPoolId :: Coin Nft }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data PoolFee = PoolFee
  { poolFeeNum' :: Integer
  , poolFeeDen' :: Integer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Pool = Pool
  { poolId        :: PoolId
  , poolReservesX :: Amount X
  , poolReservesY :: Amount Y
  , poolLiquidity :: Amount Liquidity
  , poolCoinX     :: Coin X
  , poolCoinY     :: Coin Y
  , poolCoinLq    :: Coin Liquidity
  , poolFee       :: PoolFee
  , outCollateral :: Amount Lovelace
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

feeDen :: Integer
feeDen = 1000

instance FromLedger Pool where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just PoolConfig{..}) ->
          Just $ OnChain fout Pool
            { poolId        = PoolId $ Coin  poolNft
            , poolReservesX = rx
            , poolReservesY = ry
            , poolLiquidity = lq
            , poolCoinX     = Coin poolX
            , poolCoinY     = Coin poolY
            , poolCoinLq    = Coin poolLq
            , poolFee       = PoolFee poolFeeNum feeDen
            , outCollateral = collateral
            }
        where
          rx         = Amount $ assetClassValueOf fullTxOutValue poolX
          ry         = Amount $ assetClassValueOf fullTxOutValue poolY
          rlq        = Amount $ assetClassValueOf fullTxOutValue poolLq
          lq         = maxLqCapAmount - rlq -- actual LQ emission
          collateral = if W.isAda poolX || W.isAda poolY then zero else minSafeOutputAmount
      _ -> Nothing
  parseFromLedger _ = Nothing

instance ToLedger Pool where
  toLedger Pool{..} =
      TxOutCandidate
        { txOutCandidateAddress  = validatorAddress poolInstance
        , txOutCandidateValue    = poolValue
        , txOutCandidateDatum    = Just $ Datum $ toBuiltinData poolConf
        }
    where
      nft            = unPoolId poolId
      poolLqReserves = maxLqCapAmount - poolLiquidity
      poolValue      = assetClassValue (unCoin nft) 1 <>
                       assetAmountValue (AssetAmount poolCoinLq poolLqReserves) <>
                       assetAmountValue (AssetAmount poolCoinX poolReservesX) <>
                       assetAmountValue (AssetAmount poolCoinY poolReservesY) <>
                       lovelaceValueOf (unAmount outCollateral)

      poolConf = PoolConfig
        { poolNft    = unCoin nft
        , poolX      = unCoin poolCoinX
        , poolY      = unCoin poolCoinY
        , poolLq     = unCoin poolCoinLq
        , poolFeeNum = poolFeeNum' poolFee
        }

data PoolInitError = InvalidLiquidity Integer

initPool :: S.PoolConfig -> (Amount X, Amount Y) -> Either PoolInitError (Predicted Pool, Amount Liquidity)
initPool S.PoolConfig{..} (inX, inY) = do
  unlockedLq <- fmap getAmount (initialLiquidityAmount poolLq (inX, inY))
  let
    outCollateral =
      if isAda poolX || isAda poolY
        then zero
        else minSafeOutputAmount
    pool = Pool
      { poolId        = PoolId poolNft
      , poolReservesX = inX
      , poolReservesY = inY
      , poolLiquidity = unlockedLq
      , poolCoinX     = poolX
      , poolCoinY     = poolY
      , poolCoinLq    = poolLq
      , poolFee       = PoolFee poolFeeNum feeDen
      , outCollateral = outCollateral
      }
    poolOut = toLedger pool
  Right (Predicted poolOut pool, unlockedLq)

applyDeposit :: (MonadIO f) => Pool -> (Amount X, Amount Y) -> f (Predicted Pool)
applyDeposit p@Pool{..} (inX, inY) = do
  _ <- liftIO $ print ("applyDeposit" ++ (show inX) ++ "::" ++ (show inY) ++ "::" ++ (show p))
  let unlockedLq = getAmount (liquidityAmount p (inX, inY))
  _ <- liftIO $ print ("unlockedLq" ++ (show unlockedLq))
  let nextPool = p
            { poolReservesX = poolReservesX + inX
            , poolReservesY = poolReservesY + inY
            , poolLiquidity = poolLiquidity + unlockedLq
            }
  _ <- liftIO $ print ("nextPool" ++ (show nextPool))
  let nextPoolOut = toLedger nextPool
  _ <- liftIO $ print ("nextPoolOut" ++ (show nextPoolOut))
  pure $ Predicted nextPoolOut nextPool

applyRedeem :: Pool -> Amount Liquidity -> Predicted Pool
applyRedeem p@Pool{..} burnedLq =
    Predicted nextPoolOut nextPool
  where
    (outX, outY) = bimap getAmount getAmount (sharesAmount p burnedLq)

    nextPool = p
      { poolReservesX = poolReservesX - outX
      , poolReservesY = poolReservesY - outY
      , poolLiquidity = poolLiquidity - burnedLq
      }

    nextPoolOut = toLedger nextPool

applySwap :: Pool -> AssetAmount Base -> Predicted Pool
applySwap p@Pool{..} base =
  Predicted nextPoolOut nextPool
  where
    xy             = unCoin (getAsset base) == unCoin poolCoinX
    baseAmount     = unAmount $ getAmount base
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    quoteAmount    = assetAmountRawValue (outputAmount p base)

    nextPool =
      if xy then p
        { poolReservesX = Amount $ poolReservesX' + baseAmount
        , poolReservesY = Amount $ poolReservesY' - quoteAmount
        }
      else p
        { poolReservesX = Amount $ poolReservesX' - quoteAmount
        , poolReservesY = Amount $ poolReservesY' + baseAmount
        }

    nextPoolOut = toLedger nextPool

initialLiquidityAmount :: Coin Liquidity -> (Amount X, Amount Y) -> Either PoolInitError (AssetAmount Liquidity)
initialLiquidityAmount poolCoinLq (Amount inX, Amount inY) =
  fmap (assetAmountCoinOf poolCoinLq) $ case isqrt (inX * inY) of
    Exactly l | l > 0       -> Right l
    Approximately l | l > 0 -> Right $ l + 1
    _                       -> Left $ InvalidLiquidity (inX * inY)

liquidityAmount :: Pool -> (Amount X, Amount Y) -> AssetAmount Liquidity
liquidityAmount Pool{..} (Amount inX, Amount inY) =
    assetAmountCoinOf poolCoinLq $
      min (inX * poolLiquidity' `div` poolReservesX') (inY * poolLiquidity' `div` poolReservesY')
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    poolLiquidity' = unAmount poolLiquidity

sharesAmount :: Pool -> Amount Liquidity -> (AssetAmount X, AssetAmount Y)
sharesAmount Pool{..} burnedLq =
    ( assetAmountCoinOf poolCoinX (burnedLq' * poolReservesX' `div` poolLiquidity')
    , assetAmountCoinOf poolCoinY (burnedLq' * poolReservesY' `div` poolLiquidity')
    )
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    burnedLq'      = unAmount burnedLq
    poolLiquidity' = unAmount poolLiquidity

outputAmount :: Pool -> AssetAmount Base -> AssetAmount Quote
outputAmount Pool{poolFee=PoolFee{..}, ..} (AssetAmount baseAsset (Amount baseAmount)) =
    if xy then
      assetAmountCoinOf (retagCoin poolCoinY)
        ((poolReservesY' * baseAmount * poolFeeNum') `div` (poolReservesX' * poolFeeDen' + baseAmount * poolFeeNum'))
    else
      assetAmountCoinOf (retagCoin poolCoinX)
        ((poolReservesX' * baseAmount * poolFeeNum') `div` (poolReservesY' * poolFeeDen' + baseAmount * poolFeeNum'))
  where
    xy             = unCoin baseAsset == unCoin poolCoinX
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
