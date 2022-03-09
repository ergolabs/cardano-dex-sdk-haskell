module ErgoDex.Amm.PScripts
  ( poolValidator
  , swapValidator
  , depositValidator
  , redeemValidator
  , validatorAddress
  ) where

import Plutus.V1.Ledger.Api (Validator, Address)

import qualified ErgoDex.PContracts.PPool    as PP
import qualified ErgoDex.PContracts.PSwap    as PS
import qualified ErgoDex.PContracts.PDeposit as PD
import qualified ErgoDex.PContracts.PRedeem  as PR

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1          (mkValidator)
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutarch.Unsafe          (punsafeCoerce)

import Ledger.Address (scriptHashAddress)
import Ledger.Scripts (validatorHash)

wrapValidator
  :: (PIsData dt, PIsData rdmr)
  => Term s (dt :--> rdmr :--> PScriptContext :--> PBool)
  -> Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
  let
    dt     = pfromData $ punsafeCoerce datum
    rdmr   = pfromData $ punsafeCoerce redeemer
    result = validator # dt # rdmr # ctx
  in popaque result

poolValidator :: Validator
poolValidator = mkValidator $ wrapValidator PP.poolValidatorT

swapValidator :: Validator
swapValidator = mkValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Validator
depositValidator = mkValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Validator
redeemValidator = mkValidator $ wrapValidator PR.redeemValidatorT

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash
