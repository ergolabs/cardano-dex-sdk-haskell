module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr(..)
  , mkPoolActions
  ) where

import           Control.Monad          (when)
import           Data.Bifunctor
import           Data.Tuple
import qualified Data.Set               as Set
import           Control.Exception.Base
import           RIO

import           Ledger          (PubKeyHash(..), Redeemer(..), pubKeyHashAddress, PaymentPubKeyHash(..))
import qualified Ledger.Interval as Interval
import qualified Ledger.Ada      as Ada
import           Ledger.Scripts  (unitRedeemer)
import           Ledger.Value    (assetClassValue)
import           PlutusTx        (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           ErgoDex.Amm.Scripts
import           CardanoTx.Models

data OrderExecErr =
    PriceTooHigh
  | PoolMismatch PoolId PoolId
  deriving (Show)

instance Exception OrderExecErr

data PoolActions f = PoolActions
  { runSwap    :: Confirmed Swap    -> (FullTxOut, Pool) -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
  , runDeposit :: Confirmed Deposit -> (FullTxOut, Pool) -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
  , runRedeem  :: Confirmed Redeem  -> (FullTxOut, Pool) -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
  }

mkPoolActions :: (MonadUnliftIO f, MonadIO f) => PubKeyHash -> PoolActions f
mkPoolActions executorPkh = PoolActions
  { runSwap    = runSwap' executorPkh
  , runDeposit = runDeposit' executorPkh
  , runRedeem  = runRedeem' executorPkh
  }

runSwap' :: (MonadUnliftIO f, MonadIO f) => PubKeyHash -> Confirmed Swap -> (FullTxOut, Pool) -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = do
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Swap)
    orderIn = mkScriptTxIn swapOut swapScript unitRedeemer
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applySwap pool (AssetAmount swapBase swapBaseIn)

    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

  when (swapPoolId /= poolId pool)               (throw $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (throw $ PriceTooHigh)

  let
    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash swapRewardPkh) Nothing
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress  = rewardAddr
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          }
      where
        initValue     = fullTxOutValue swapOut
        exFee         = assetAmountRawValue quoteOutput * exFeePerTokenNum `div` exFeePerTokenDen
        residualValue =
             initValue
          <> assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn) -- Remove Base input
          <> Ada.lovelaceValueOf exFee                                        -- Remove Fee

        rewardValue = assetAmountValue quoteOutput <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress (PaymentPubKeyHash executorPkh) Nothing
      , txCandidateValidRange   = Interval.always
      }

  RIO.try $ RIO.evaluate (txCandidate, pp)

runDeposit' :: (MonadUnliftIO f, MonadIO f) => PubKeyHash -> Confirmed Deposit -> (FullTxOut, Pool) -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (poolOut, pool@Pool{..}) = do
  _ <- liftIO $ print "runDeposit1"
  when (depositPoolId /= poolId) (throw $ PoolMismatch depositPoolId poolId)
  _ <- liftIO $ print "runDeposit2"
  _ <- liftIO $ print "poolOut: "
  poolIn  <- RIO.evaluate $ mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Deposit)
  _ <- liftIO $ print ("runDeposit3" ++ (show poolIn))
  orderIn <- RIO.evaluate $ mkScriptTxIn depositOut depositScript unitRedeemer
  _ <- liftIO $ print ("runDeposit4" ++ (show orderIn))
  inputs  <- RIO.evaluate $ [orderIn, poolIn]
  _ <- liftIO $ print ("runDeposit5" ++ (show inputs))
  let (inX :: Amount X, inY :: Amount Y) =
          bimap entryAmount entryAmount $
            if assetEntryClass (fst depositPair) == unCoin poolCoinX
            then depositPair
            else swap depositPair
              where entryAmount (AssetEntry (_, v)) = Amount v
  _ <- liftIO $ print ("runDeposit6" ++ (show inX) ++ "::" ++ (show inY))
  exFee <- RIO.evaluate $ unExFee depositExFee
  _ <- liftIO $ print ("runDeposit7" ++ (show exFee))
  let (netInX, netInY)
         | isAda poolCoinX = (inX - retagAmount exFee - retagAmount adaCollateral, inY)
         | isAda poolCoinY = (inX, inY - retagAmount exFee - retagAmount adaCollateral)
         | otherwise       = (inX, inY)
  _ <- liftIO $ print ("runDeposit8" ++ (show netInX) ++ "::" ++ (show netInY))
  pp@(Predicted nextPoolOut _) <- applyDeposit pool (netInX, netInY)
  _ <- liftIO $ print ("runDeposit8" ++ (show pp))
  mintLqValue <- RIO.evaluate $ assetAmountValue (liquidityAmount pool (netInX, netInY))
  _ <- liftIO $ print ("runDeposit9" ++ (show mintLqValue))
  rewardAddr <- RIO.evaluate $ pubKeyHashAddress (PaymentPubKeyHash depositRewardPkh) Nothing
  _ <- liftIO $ print ("runDeposit10" ++ (show rewardAddr))
  let rewardOut  =
         TxOutCandidate
           { txOutCandidateAddress = rewardAddr
           , txOutCandidateValue   = rewardValue
           , txOutCandidateDatum   = Nothing
           }
         where
           initValue     = fullTxOutValue depositOut
           residualValue =
                initValue
             <> assetClassValue (unCoin poolCoinX) (negate $ unAmount netInX) -- Remove X net input
             <> assetClassValue (unCoin poolCoinY) (negate $ unAmount netInY) -- Remove Y net input
             <> Ada.lovelaceValueOf (negate $ unAmount exFee)                 -- Remove Fee
           rewardValue = residualValue <> mintLqValue
  _ <- liftIO $ print ("runDeposit11" ++ (show rewardOut))
  _ <- liftIO $ print ("======================================")
  _ <- liftIO $ print ("nextPoolOut: " ++ (show nextPoolOut))
  _ <- liftIO $ print ("======================================")
  _ <- liftIO $ print ("rewardOut: " ++ (show rewardOut))
  _ <- liftIO $ print ("======================================")
  txCandidate <- RIO.evaluate $ TxCandidate
                     { txCandidateInputs       = Set.fromList inputs
                     , txCandidateOutputs      = [nextPoolOut, rewardOut]
                     , txCandidateValueMint    = mempty
                     , txCandidateMintInputs   = mempty
                     , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress (PaymentPubKeyHash executorPkh) Nothing
                     , txCandidateValidRange   = Interval.always
                     }

  RIO.try $ RIO.evaluate (txCandidate, pp)

runRedeem' :: (MonadUnliftIO f, MonadIO f) => PubKeyHash -> Confirmed Redeem -> (FullTxOut, Pool) -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
runRedeem' executorPkh (Confirmed redeemIn Redeem{..}) (poolOut, pool@Pool{..}) = do
  when (redeemPoolId /= poolId) (throw $ PoolMismatch redeemPoolId poolId)
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Redeem)
    orderIn = mkScriptTxIn redeemIn redeemScript unitRedeemer
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applyRedeem pool redeemLqIn

    burnLqValue = assetClassValue (unCoin redeemLq) (negate $ unAmount redeemLqIn)

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash redeemRewardPkh) Nothing
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = rewardValue
          , txOutCandidateDatum     = Nothing
          }
      where
        (outX, outY)  = sharesAmount pool redeemLqIn
        initValue     = fullTxOutValue redeemIn
        exFee         = Ada.lovelaceValueOf $ negate $ unAmount $ unExFee redeemExFee
        residualValue = initValue <> burnLqValue <> exFee -- Remove LQ input and ExFee

        rewardValue = assetAmountValue outX <> assetAmountValue outY <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress (PaymentPubKeyHash executorPkh) Nothing
      , txCandidateValidRange   = Interval.always
      }

  RIO.try $ RIO.evaluate (txCandidate, pp)