module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr(..)
  , mkPoolActions
  ) where

import           Control.Exception.Base
import           Control.Monad          (when)
import qualified Data.Set               as Set
import           Data.Bifunctor
import           Data.Tuple

import           Ledger          (Redeemer(..), PaymentPubKeyHash(..), pubKeyHashAddress)
import qualified Ledger.Interval as Interval
import qualified Ledger.Ada      as Ada
import           Ledger.Value    (assetClassValue)
import           PlutusTx        (toBuiltinData)
import           Ledger.Scripts  (Validator)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool        as P
import qualified ErgoDex.Contracts.Proxy.Order as O
import           ErgoDex.Contracts.Types
import ErgoDex.PValidators
    ( depositValidator, poolValidator, redeemValidator, swapValidator )
import           CardanoTx.Models

data OrderExecErr
  = PriceTooHigh
  | PoolMismatch PoolId PoolId
  | EmptyPool PoolId
  | PoolNotFoundInFinalTx PoolId
  deriving (Show)

instance Exception OrderExecErr

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PaymentPubKeyHash -> PoolActions
mkPoolActions executorPkh = PoolActions
  { runSwap    = runSwap' executorPkh
  , runDeposit = runDeposit' executorPkh
  , runRedeem  = runRedeem' executorPkh
  }

newtype PoolIn  = PoolIn FullTxOut
newtype OrderIn = OrderIn FullTxOut

mkOrderInputs :: P.PoolAction -> Validator -> PoolIn -> OrderIn -> Set.Set FullTxIn
mkOrderInputs action orderValidator (PoolIn poolOut) (OrderIn orderOut) =
    Set.fromList [poolIn, orderIn]
  where
    preInputs = Set.fromList [poolOut, orderOut]
    poolIx    = toInteger $ Set.findIndex poolOut preInputs
    orderIx   = toInteger $ Set.findIndex orderOut preInputs
    poolIn    = mkScriptTxIn poolOut poolValidator   (Redeemer $ toBuiltinData $ P.PoolRedeemer action poolIx)
    orderIn   = mkScriptTxIn orderOut orderValidator (Redeemer $ toBuiltinData $ O.OrderRedeemer poolIx orderIx 1 O.Apply)

runSwap' :: PaymentPubKeyHash -> Confirmed Swap -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = do
  let
    inputs = mkOrderInputs P.Swap swapValidator (PoolIn poolOut) (OrderIn swapOut)

    pp@(Predicted nextPoolOut _) = applySwap pool (AssetAmount swapBase swapBaseIn)

    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

  when (swapPoolId /= poolId pool)               (Left $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)

  let
    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash swapRewardPkh) swapRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = EmptyDatum
          }
      where
        initValue     = fullTxOutValue swapOut
        exFee         = assetAmountRawValue quoteOutput * exFeePerTokenNum `div` exFeePerTokenDen
        residualValue =
             initValue
          <> assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn) -- Remove Base input
          <> Ada.lovelaceValueOf (negate exFee)                               -- Remove Fee

        rewardValue = assetAmountValue quoteOutput <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp)

runDeposit' :: PaymentPubKeyHash -> Confirmed Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (poolOut, pool@Pool{..}) = do
  when (depositPoolId /= poolId) (Left $ PoolMismatch depositPoolId poolId)
  let
    inputs = mkOrderInputs P.Deposit depositValidator (PoolIn poolOut) (OrderIn depositOut)

    (inX, inY) =
        bimap entryAmount entryAmount $
          if assetEntryClass (fst depositPair) == unCoin poolCoinX
          then depositPair
          else swap depositPair
      where entryAmount (AssetEntry (_, v)) = Amount v

    exFee = unExFee depositExFee

    (netInX, netInY)
      | isAda poolCoinX = (inX - retagAmount exFee - retagAmount adaCollateral, inY)
      | isAda poolCoinY = (inX, inY - retagAmount exFee - retagAmount adaCollateral)
      | otherwise       = (inX, inY)

    (unlockedLq, (Amount changeX, Amount changeY)) = rewardLp pool (netInX, netInY)

    alignmentValue =
         assetClassValue (unCoin poolCoinY) changeY
      <> assetClassValue (unCoin poolCoinX) changeX

    pp@(Predicted nextPoolOut _) = applyDeposit pool (netInX, netInY)

    mintLqValue = assetAmountValue (AssetAmount poolCoinLq unlockedLq)

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash depositRewardPkh) depositRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = EmptyDatum
          }
      where
        initValue     = fullTxOutValue depositOut
        residualValue =
             initValue
          <> assetClassValue (unCoin poolCoinX) (negate $ unAmount netInX) -- Remove X net input
          <> assetClassValue (unCoin poolCoinY) (negate $ unAmount netInY) -- Remove Y net input
          <> Ada.lovelaceValueOf (negate $ unAmount exFee)                 -- Remove Fee
        rewardValue = residualValue <> mintLqValue <> alignmentValue

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp)

runRedeem' :: PaymentPubKeyHash -> Confirmed Redeem -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runRedeem' executorPkh (Confirmed redeemOut Redeem{..}) (poolOut, pool@Pool{..}) = do
  when (redeemPoolId /= poolId) (Left $ PoolMismatch redeemPoolId poolId)
  let
    inputs = mkOrderInputs P.Redeem redeemValidator (PoolIn poolOut) (OrderIn redeemOut)

    pp@(Predicted nextPoolOut _) = applyRedeem pool redeemLqIn

    burnLqValue = assetClassValue (unCoin redeemLq) (negate $ unAmount redeemLqIn)

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash redeemRewardPkh) redeemRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = EmptyDatum
          }
      where
        (outX, outY)  = sharesAmount pool redeemLqIn
        initValue     = fullTxOutValue redeemOut
        exFee         = Ada.lovelaceValueOf $ negate $ unAmount $ unExFee redeemExFee
        residualValue = initValue <> burnLqValue <> exFee -- Remove LQ input and ExFee

        rewardValue = assetAmountValue outX <> assetAmountValue outY <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp)
