{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Tests where

import Plutarch.Api.V1
import PExtra.API
import Plutarch.Prelude
import PExtra.Monadic
import Plutarch

import qualified ErgoDex.Contracts.Pool as Pool
import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.PDeposit as PDeposit

import Models.PGenerator
import Models.Generator
import Models.Utils

runSuccessDeposite :: Term s PUnit
runSuccessDeposite = phoistAcyclic $ unTermCont $ do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    pnft = pgenAssetClass nftTn cs
    px   = pgenAssetClass xTn cs
    py   = pgenAssetClass yTn cs
    plq  = pgenAssetClass lqTn cs

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    ppoolConfig = pconstant poolConfig
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    pOrderConfig = pgenDepositConfig # pnft # px # py # plq # 100 # pconstant pubKeyHashReward # 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRef poolInOut
    orderInIn  = pgenPoolIn genTxOutRef orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRef
    cxt     = pgenContext txInfo purpose

    poolRedeemer = pgenPoolRedeemer # pcon PPool.Deposit # 0
    orderRedeem  = pgenOrderRedeem # 0 # 1 # 1

    runPool    = PPool.poolValidatorT # ppoolConfig # poolRedeemer # cxt
    runDeposit = PDeposit.depositValidatorT # pOrderConfig # orderRedeem # cxt

  res1 <- tlet $ pif (runPool) (pcon PUnit) (ptraceError "incorrect pool") -- true
  res2 <- tlet $ pif (runDeposit) (pcon PUnit) (ptraceError "incorrect deposit") -- true

  return res1

runFailureDepositeIcorrectPoolRedeemer :: Term s PUnit
runFailureDepositeIcorrectPoolRedeemer = phoistAcyclic $ unTermCont $ do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    pnft = pgenAssetClass nftTn cs
    px   = pgenAssetClass xTn cs
    py   = pgenAssetClass yTn cs
    plq  = pgenAssetClass lqTn cs

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    ppoolConfig = pconstant poolConfig
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    pOrderConfig = pgenDepositConfig # pnft # px # py # plq # 100 # pconstant pubKeyHashReward # 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRef poolInOut
    orderInIn  = pgenPoolIn genTxOutRef orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRef
    cxt     = pgenContext txInfo purpose

    poolRedeemer = pgenPoolRedeemer # pcon PPool.Swap # 1 -- doesnt fail if PPool action is incorrect
    orderRedeem  = pgenOrderRedeem # 0 # 1 # 1 -- doesnt fail if 2nd index is incorrect

    runPool    = PPool.poolValidatorT # ppoolConfig # poolRedeemer # cxt
    runDeposit = PDeposit.depositValidatorT # pOrderConfig # orderRedeem # cxt

  -- res1 <- tlet $ pif (runPool) (pcon PUnit) (ptraceError "incorrect pool") -- true
  res2 <- tlet $ pif (runDeposit) (pcon PUnit) (ptraceError "incorrect deposit") -- true

  return res2

runFailureDepositePoolAndPoolValueAreDifferent :: Term s PUnit
runFailureDepositePoolAndPoolValueAreDifferent = phoistAcyclic $ unTermCont $ do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    pnft = pgenAssetClass nftTn cs
    px   = pgenAssetClass xTn cs
    py   = pgenAssetClass yTn cs
    plq  = pgenAssetClass lqTn cs

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    wrongNft = genAssetClass cs (genTokenName genNftWrong)
    pWrongNft = pgenAssetClass (genTokenName genNftWrong) cs

    poolConfig  = genPoolConfig wrongNft x y lq 100
    ppoolConfig = pconstant poolConfig
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig wrongNft x y lq 100 pubKeyHashReward 100
    pOrderConfig = pgenDepositConfig # pWrongNft # px # py # plq # 100 # pconstant pubKeyHashReward # 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRef poolInOut
    orderInIn  = pgenPoolIn genTxOutRef orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRef
    cxt     = pgenContext txInfo purpose

    poolRedeemer = pgenPoolRedeemer # pcon PPool.Swap # 1 -- doesnt fail if PPool action is incorrect
    orderRedeem  = pgenOrderRedeem # 0 # 1 # 1 -- doesnt fail if 2nd index is incorrect

    runPool    = PPool.poolValidatorT # ppoolConfig # poolRedeemer # cxt
    runDeposit = PDeposit.depositValidatorT # pOrderConfig # orderRedeem # cxt

  res1 <- tlet $ pif (runPool) (pcon PUnit) (ptraceError "incorrect pool") -- true
  -- res2 <- tlet $ pif (runDeposit) (pcon PUnit) (ptraceError "incorrect deposit") -- true

  return res1