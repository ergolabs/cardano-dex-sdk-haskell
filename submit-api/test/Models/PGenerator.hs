{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Models.PGenerator where

import Plutarch.Api.V1
import PExtra.API
import Plutarch.Prelude

import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts.Validators as LV

import qualified ErgoDex.PContracts.PDeposit as PDeposit
import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.POrder as POrder 

import qualified ErgoDex.Amm.PScripts as PScripts

pgenAssetClass :: TokenName -> CurrencySymbol -> Term s PAssetClass
pgenAssetClass tn cs = phoistAcyclic $
  pcon $
    PAssetClass $
      pdcons
        # pdata (pconstant cs) #$ pdcons
        # pdata (pconstant tn) #$ pdnil

pgenDepositConfig :: Term s (PAssetClass :--> PAssetClass :--> PAssetClass :--> PAssetClass :--> PInteger :--> PPubKeyHash :--> PInteger :--> PDeposit.DepositConfig)
pgenDepositConfig = phoistAcyclic $ plam $ \nft x y lq fee pkh cFee ->
  pcon $
    PDeposit.DepositConfig $
      pdcons
        # pdata nft #$ pdcons
        # pdata x #$ pdcons
        # pdata y #$ pdcons
        # pdata lq #$ pdcons
        # pdata fee #$ pdcons
        # pdata pkh #$ pdcons
        # pdata cFee #$ pdnil

pgenPoolConfig :: Term s (PAssetClass :--> PAssetClass :--> PAssetClass :--> PAssetClass :--> PInteger :--> PPool.PoolConfig)
pgenPoolConfig = phoistAcyclic $ plam $ \nft x y lq fee ->
  pcon $
    PPool.PoolConfig $
      pdcons
        # pdata nft #$ pdcons
        # pdata x #$ pdcons
        # pdata y #$ pdcons
        # pdata lq #$ pdcons
        # pdata fee #$ pdnil

pgenPoolRedeemer :: Term s (PPool.PoolAction :--> PInteger :--> PPool.PoolRedeemer)
pgenPoolRedeemer = phoistAcyclic $ plam $ \action ix ->
  pcon $
    PPool.PoolRedeemer $
      pdcons
        # pdata action #$ pdcons
        # pdata ix #$ pdnil

pgenOrderRedeem :: Term s (PInteger :--> PInteger :--> PInteger :--> POrder.OrderRedeemer)
pgenOrderRedeem = phoistAcyclic $ plam $ \a b c ->
  pcon $
    POrder.OrderRedeemer $
      pdcons
        # pdata a #$ pdcons
        # pdata b #$ pdcons
        # pdata c #$ pdnil

pgenPoolIn :: TxOutRef -> TxOut -> TxInInfo
pgenPoolIn ref out =
  TxInInfo
    { txInInfoOutRef   = ref
    , txInInfoResolved = out
    }

pgenPoolOut :: DatumHash -> Value -> ValidatorHash -> TxOut
pgenPoolOut dh v vh =
  TxOut
    { txOutAddress   = Address (ScriptCredential vh) Nothing
    , txOutValue     = v
    , txOutDatumHash = Just dh
    }

pgenOrderOut :: DatumHash -> Value -> PubKeyHash -> TxOut
pgenOrderOut dh v pkh =
  TxOut
    { txOutAddress   = Address (PubKeyCredential pkh) Nothing
    , txOutValue     = v
    , txOutDatumHash = Just dh
    }

pgenTxInfo :: TxInInfo -> TxInInfo -> TxOut -> TxOut -> TxInfo
pgenTxInfo pIn oIn pOut oOut =
  TxInfo
    { txInfoInputs = [pIn, oIn]
    , txInfoOutputs = [pOut, oOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = []
    , txInfoId = "b0"
    }

pgenContext :: TxInfo -> ScriptPurpose -> Term s PScriptContext
pgenContext cxt purpose =
  pconstant
    (ScriptContext cxt purpose)

pgenPurpose :: TxOutRef -> ScriptPurpose
pgenPurpose = Spending

pgenPoolValidator :: ValidatorHash
pgenPoolValidator = LV.validatorHash $ LV.unsafeMkTypedValidator $ PScripts.poolValidator

pgenDepositValidator :: ValidatorHash
pgenDepositValidator = LV.validatorHash $ LV.unsafeMkTypedValidator $ PScripts.depositValidator