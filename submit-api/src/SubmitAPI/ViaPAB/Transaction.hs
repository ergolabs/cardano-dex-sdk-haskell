module SubmitAPI.ViaPAB.Transaction where

import qualified Data.Set            as Set
import qualified Data.Map            as Map
import           Data.Functor

import qualified CardanoTx.Models            as Sdk
import qualified Ledger                      as P
import qualified Ledger.Constraints          as P
import qualified Ledger.Constraints.OffChain as P
import           Ledger.Constraints.OffChain (UnbalancedTx(unBalancedTxTx))
import qualified Plutus.V1.Ledger.Credential as P

import CardanoTx.ToPlutus (ToPlutus(toPlutus))

--mkUnbalancedTx :: Set.Set Sdk.FullCollateralTxIn -> Sdk.TxCandidate -> P.UnbalancedTx
--mkUnbalancedTx collateral tx@Sdk.TxCandidate{..} =
--    P.UnbalancedTx
--      { unBalancedTxTx                  = mkPlutusTx collateral tx
--      , unBalancedTxRequiredSignatories = collectKeys allInputs
--      , unBalancedTxUtxoIndex           = Map.fromList $ allInputs <&> (\fout -> (Sdk.fullTxOutRef fout, toPlutus fout))
--      , unBalancedTxValidityTimeRange   = P.always -- todo
--      }
--  where
--    inputs    = Set.elems txCandidateInputs <&> Sdk.fullTxInTxOut
--    cinputs   = Set.elems collateral <&> Sdk.fullCollateralTxInTxOut
--    allInputs = inputs ++ cinputs

mkPlutusTx :: Set.Set Sdk.FullCollateralTxIn -> Sdk.TxCandidate -> P.Tx
mkPlutusTx collateral Sdk.TxCandidate{..} =
  P.Tx
    { txInputs      = Set.fromList $ Set.elems txCandidateInputs <&> toPlutus
    , txCollateral  = Set.fromList $ Set.elems collateral <&> toPlutus
    , txOutputs     = txCandidateOutputs <&> toPlutus
    , txMint        = Sdk.unMintValue txCandidateValueMint
    , txFee         = mempty
    , txValidRange  = txCandidateValidRange
    , txMintScripts = Sdk.mintInputsPolicies txCandidateMintInputs
    , txSignatures  = mempty
    , txRedeemers   = mempty -- todo
    , txData        = collectOutputsData txCandidateOutputs
    }

collectKeys :: [Sdk.FullTxOut] -> Map.Map P.PaymentPubKeyHash (Maybe P.PaymentPubKey)
collectKeys inputs = Map.fromList $ inputs >>= extractPkh
  where
    extractPkh Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _} = [(P.PaymentPubKeyHash pkh, Nothing)]
    extractPkh _ = []

collectOutputsData :: [Sdk.TxOutCandidate] -> Map.Map P.DatumHash P.Datum
collectOutputsData outputs =
    Map.fromList $ outputs >>= tryGetDatum
  where
    tryGetDatum Sdk.TxOutCandidate{txOutCandidateDatum=Just d} = pure (P.datumHash d, d)
    tryGetDatum _                                              = mempty
