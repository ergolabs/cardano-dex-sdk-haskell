{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}

module Models.Generator
  ( genTxOutRef
  , genTokenName
  , genCurrencySymbol
  , genAssetClass
  , genValue
  , genValues
  , genAdaValue
  , genPoolConfig
  , genDepositConfig
  , genDatum
  , genDatumHash
  , genOrderDatum
  , genPoolRedeemer
  , genRedeemer
  , genMaxLq
  -- , genPoolValue
  , genTxInType
  , genScriptCredential
  , genFullTxOut
  , genTxIn
  , genTxOutCandidate
  , pubKeyHashReward
  , genTxCandidate
  ) where

import Models.Utils

import qualified ErgoDex.Contracts.Pool as P
import qualified ErgoDex.Contracts.Proxy.Deposit  as Deposit
import qualified ErgoDex.Contracts.Proxy.Order as Order
import ErgoDex.Amm.Scripts ()
import qualified ErgoDex.Amm.PScripts as PScript
import CardanoTx.Models

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass(..))
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusTx.AssocMap as Map
import qualified Ledger.Ada      as Ada
import qualified Ledger as Ledger
import qualified Ledger.Typed.Scripts.Validators as LV
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified PlutusTx
import qualified Ledger.Interval as Interval

import qualified Data.Set as Set
import qualified Data.ByteString         as BS
import GHC.Generics

genTokenName :: BS.ByteString -> TokenName
genTokenName = TokenName . BuiltinByteString

genCurrencySymbol :: BS.ByteString -> CurrencySymbol
genCurrencySymbol = CurrencySymbol . BuiltinByteString

genAssetClass :: CurrencySymbol -> TokenName -> AssetClass
genAssetClass cs tn = AssetClass (cs, tn)

genValue :: AssetClass -> Integer -> Value
genValue (AssetClass (cs, tn)) qty = Value $ Map.fromList [(cs, Map.singleton tn qty)]

genAdaValue :: Integer -> Value
genAdaValue qty = genValue (genAssetClass Ada.adaSymbol Ada.adaToken) qty

genValues :: [Value] -> Value -> Value
genValues (x:xs) acc = genValues xs (x <> acc)
genValues [] acc = acc

genPoolConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> P.PoolConfig
genPoolConfig nft x y lq fee = P.PoolConfig nft x y lq fee

genDepositConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> Integer -> Deposit.DepositConfig
genDepositConfig nft x y lq fee pkh cFee = Deposit.DepositConfig nft x y lq fee pkh cFee

genDatum :: P.PoolConfig -> Datum
genDatum = Datum . toBuiltinData

genOrderDatum :: Deposit.DepositConfig -> Datum
genOrderDatum = Datum . toBuiltinData

genDatumHash :: Datum -> DatumHash
genDatumHash datum = Ledger.datumHash datum

genPoolRedeemer :: Integer -> P.PoolAction -> P.PoolRedeemer
genPoolRedeemer ix action = P.PoolRedeemer action ix

genDepositRedeemer :: Integer -> Integer -> Integer -> Order.OrderRedeemer
genDepositRedeemer a b c = Order.OrderRedeemer a b c

genDepositRedeemerR :: Order.OrderRedeemer -> Redeemer
genDepositRedeemerR = Redeemer . toBuiltinData

genRedeemer :: P.PoolRedeemer -> Redeemer
genRedeemer = Redeemer . toBuiltinData

genMaxLq :: Integer
genMaxLq = 0x7fffffffffffffff

genTxInType :: Datum -> Redeemer -> Ledger.TxInType
genTxInType datum redeemer = Ledger.ConsumeScriptAddress PScript.poolValidator redeemer datum 

genScriptCredential :: Credential
genScriptCredential = ScriptCredential $ LV.validatorHash $ LV.unsafeMkTypedValidator $ PScript.poolValidator

genFullTxOut :: TxOutRef -> Value -> Datum -> FullTxOut
genFullTxOut ref value datum =
  let
    address   = Address genScriptCredential Nothing
    datumHash = genDatumHash datum
  in
    FullTxOut
      { fullTxOutRef = ref
      , fullTxOutAddress = address
      , fullTxOutValue = value
      , fullTxOutDatumHash = Just datumHash
      , fullTxOutDatum = Just datum
      }

genTxCandidate :: TxCandidate
genTxCandidate =
  let
    nft = genAssetClass (genCurrencySymbol genCS) (genTokenName genNft)
    x   = genAssetClass (genCurrencySymbol genCS) (genTokenName genX)
    y   = genAssetClass (genCurrencySymbol genCS) (genTokenName genY)
    lq  = genAssetClass (genCurrencySymbol genCS) (genTokenName genLQ)

    nftV = genValue nft 1
    xV   = genValue x 10
    yV   = genValue y 10
    lqV  = genValue lq (genMaxLq - 10)
    adaV = genAdaValue 100000

    values = genValues [nftV, xV, yV, lqV, adaV] mempty

    poolCfg = genPoolConfig nft x y lq 100

    poolRedeemer = genPoolRedeemer 0 P.Deposit

    datum = genDatum poolCfg

    redeemer = genRedeemer poolRedeemer

    ref = genTxOutRef

    txOut = genFullTxOut ref values datum

    txIn = genTxIn txOut datum redeemer

    xVOrder = genValue x 10
    yVOrder = genValue y 10

    orderAdaV = genAdaValue 100000

    orderValues = genValues [xVOrder, yVOrder, orderAdaV] mempty

    orderConfig = genDepositConfig nft x y lq 100 pubKeyHashReward 100

    orderDatum = genOrderDatum orderConfig

    orderRedeemer = genDepositRedeemerR $ genDepositRedeemer 0 1 1

    orderTxOut = genFullTxOut ref orderValues orderDatum

    orderTxIn = genTxIn orderTxOut orderDatum orderRedeemer

    resultPoolValue = genValues [xVOrder, yVOrder, (genValue lq (-10))] values
    txOutCandidate = genTxOutCandidate resultPoolValue datum

    resultRewardValue = genValues [genValue lq 10, genAdaValue 99800] mempty
    txOutCandidateReward = genTxOutCandidate resultRewardValue orderDatum

  in
    TxCandidate 
      { txCandidateInputs       = Set.fromList [txIn, orderTxIn],
        txCandidateOutputs      = [txOutCandidate, txOutCandidateReward],
        txCandidateValueMint    = mempty,
        txCandidateMintInputs   = mempty,
        txCandidateChangePolicy = Just $ ReturnTo (Address (PubKeyCredential pubKeyHashReward) Nothing),
        txCandidateValidRange   = Interval.always,
        txCandidateSigners      = []
      } 

genTxIn :: FullTxOut -> Datum -> Redeemer -> FullTxIn
genTxIn txOut datum redeemer =
  let
    txInType = genTxInType datum redeemer
  in
    FullTxIn
      { fullTxInTxOut = txOut
      , fullTxInType  = txInType
      }

genTxOutCandidate :: Value -> Datum -> TxOutCandidate
genTxOutCandidate value datum =
  let
    address = Address genScriptCredential Nothing
  in
    TxOutCandidate
        { txOutCandidateAddress = address
        , txOutCandidateValue   = value
        , txOutCandidateDatum   = Just datum
        }

pubKeyHashReward :: PubKeyHash
pubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"