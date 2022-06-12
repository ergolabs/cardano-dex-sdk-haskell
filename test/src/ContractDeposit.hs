module ContractDeposit where

import qualified Data.Set as Set

import TokenNames
import Utils
import qualified ErgoDex.PValidators as PSc
import Generator
import ErgoDex.Amm.Orders
import ErgoDex.Types
import TxSender
import ErgoDex.State
import ErgoDex.Amm.Pool
import qualified ErgoDex.PContracts.PDeposit as PD
import qualified Ledger.Ada        as Ada

import           CardanoTx.Models                 (FullTxOut(..), FullTxIn(..), TxCandidate(..), TxOutCandidate(..), ChangePolicy(..))
import           ErgoDex.Contracts.Typed          (PoolConfig(..))
import           ErgoDex.Contracts.Types          (Coin(..), Amount(..))
import qualified ErgoDex.Amm.PoolSetup            as PS
import           ErgoDex.Contracts.Class
import           ErgoDex.Contracts.Proxy.OffChain

import           Plutus.V1.Ledger.Address         (pubKeyHashAddress)
import           Plutus.V1.Ledger.Value           (AssetClass(..), valueOf)
import           Plutus.V1.Ledger.Api             (Datum(..), toBuiltinData, DatumHash(..), TxOutRef, Value)
import           Plutus.V1.Ledger.Tx              (TxInType(ConsumePublicKeyAddress))
import qualified Ledger.Interval                  as Interval
import           CardanoTx.Models
import qualified Ledger                           as Ledger
import           Ledger.Typed.Scripts.Validators

wallet3PaymentPubKeyHash :: Ledger.PaymentPubKeyHash
wallet3PaymentPubKeyHash = Ledger.PaymentPubKeyHash pPubKeyHashReward

returnAddr :: Ledger.Address
returnAddr = pubKeyHashAddress pPubKeyHashReward

testAddr :: Ledger.Address
testAddr = pubKeyHashAddress testPubKeyHashReward

poolInput :: FullTxIn
poolInput =   
  FullTxIn
    { fullTxInTxOut = prevPoolOutput
    , fullTxInType = Ledger.ConsumeScriptAddress (PSc.poolValidator) poolRedeemer poolConfigDatum
    }

prevPoolTxRef :: TxOutRef
prevPoolTxRef = mkTxOutRef' "9f312759c5d390aaec1184361a7781c693f85aae5203d444f18ef605055068f2" 1

poolAddress :: Ledger.Address
poolAddress = Ledger.scriptAddress PSc.poolValidator

prevPoolOutput :: FullTxOut
prevPoolOutput =
  FullTxOut
    { fullTxOutRef = prevPoolTxRef
    , fullTxOutAddress = poolAddress
    , fullTxOutValue =
           (mkTokenValue' currencySymbolName nftTn 1)
        <> (mkTokenValue' currencySymbolName lpTn 9223372036854775797)
        <> (mkTokenValue' currencySymbolName xTn 10)
        <> (mkTokenValue' currencySymbolName yTn 10)
        <> (mkTokenValue' Ada.adaSymbol Ada.adaToken 5000000)
    , fullTxOutDatum = KnownDatum poolConfigDatum
    }
    
depositInput :: FullTxIn
depositInput =   
  FullTxIn
    { fullTxInTxOut = prevDepositOutput
    , fullTxInType = Ledger.ConsumeScriptAddress (PSc.depositValidator) orderRedeemer newDepositConfigDatum
    }

prevDepositRef :: TxOutRef
prevDepositRef = mkTxOutRef' "9f312759c5d390aaec1184361a7781c693f85aae5203d444f18ef605055068f2" 2

depositAddress :: Ledger.Address
depositAddress = Ledger.scriptAddress PSc.depositValidator

prevDepositOutput :: FullTxOut
prevDepositOutput =
  FullTxOut
    { fullTxOutRef = prevDepositRef
    , fullTxOutAddress = depositAddress
    , fullTxOutValue =
           (mkTokenValue' currencySymbolName xTn 10)
        <> (mkTokenValue' currencySymbolName yTn 10)
        <> (mkTokenValue' Ada.adaSymbol Ada.adaToken 5000000)
    , fullTxOutDatum = KnownDatum newDepositConfigDatum
    }

confirmedDeposit :: Confirmed Deposit
confirmedDeposit = let
  depositFullTxOut = prevDepositOutput
  deposit = Deposit
              { depositPoolId    = PoolId . Coin . AssetClass $ (currencySymbolName, nftTn)
              , depositPair      = (mkAssetEntry (AssetClass (currencySymbolName, xTn)) 10, mkAssetEntry (AssetClass (currencySymbolName, yTn)) 10)
              , depositExFee     = ExFee . Amount $ 2000000
              , depositRewardPkh = pPubKeyHashReward
              , adaCollateral    = Amount 1
              }
  in Confirmed depositFullTxOut deposit

poolTuple :: (FullTxOut, Pool)
poolTuple = let
    poolOutput = prevPoolOutput
    pool = Pool
             { poolId        = PoolId . Coin . AssetClass $ (currencySymbolName, nftTn)
             , poolReservesX = Amount 10
             , poolReservesY = Amount 10
             , poolLiquidity = Amount 10
             , poolCoinX     = Coin . AssetClass $ (currencySymbolName, xTn)
             , poolCoinY     = Coin . AssetClass $ (currencySymbolName, yTn)
             , poolCoinLq    = Coin . AssetClass $ (currencySymbolName, lpTn)
             , poolFee       = PoolFee 1 1
             , outCollateral = 5000000
             }
  in (poolOutput, pool)
