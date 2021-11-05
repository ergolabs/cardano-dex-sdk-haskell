module SubmitAPI.Service where

import           RIO
import qualified Data.Set              as Set
import qualified Data.ByteString.Char8 as B8
import           Data.Word             (Word64)
import           GHC.Natural           (naturalToInteger)

import qualified CardanoTx.Models            as Sdk
import qualified Cardano.Api                 as C
import qualified Ledger                      as P
import qualified PlutusTx.Builtins.Internal  as P
import qualified Ledger.Ada                  as P
import qualified Plutus.V1.Ledger.Credential as P
import qualified Plutus.Contract.CardanoAPI  as Interop

import SubmitAPI.Config
import SubmitAPI.Internal.Transaction
import NetworkAPI.Service
import NetworkAPI.Env
import WalletAPI.Vault
import SubmitAPI.Config

data SubmitService f = SubmitService
  { finalizeTx :: Sdk.TxCandidate -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService
  :: MonadThrow f
  => NetworkParams f
  -> Vault f
  -> TxAssemblyConfig
  -> SubmitService f
mkSubmitService network wallet conf = SubmitService
  { finalizeTx = finalizeTx' network wallet conf
  , submitTx   = undefined
  }

finalizeTx'
  :: MonadThrow f
  => NetworkParams f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' NetworkParams{..} wallet@Vault{..} conf@TxAssemblyConfig{..} txc@(Sdk.TxCandidate{..}) = do
  sysenv      <- getSystemEnv
  collaterals <- mkCollaterals wallet sysenv conf txc

  let
    isBalancedTx = amountIn == amountOut
      where
        amountIn =
          foldr (\txIn -> \acc -> (Sdk.fullTxOutValue $ Sdk.fullTxInTxOut txIn) <> acc) mempty (Set.elems txCandidateInputs)
        amountOut =
          foldr (\txOut -> \acc -> (Sdk.txOutCandidateValue txOut) <> acc) mempty txCandidateOutputs
  (C.BalancedTxBody txb _ _) <- case txCandidateChangePolicy of
    Just (Sdk.ReturnTo changeAddr) -> buildBalancedTx sysenv (Sdk.ChangeAddress changeAddr) collaterals txc
    _ | isBalancedTx               -> buildBalancedTx sysenv dummyAddr collaterals txc

  let
    requiredSigners = (Set.elems txCandidateInputs) >>= getPkh
      where
        getPkh Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _}} = [pkh]
        getPkh _                                                                                                = []

  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) requiredSigners

  pure $ signTx txb signers

mkCollaterals
  :: MonadThrow f
  => Vault f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
mkCollaterals wallet sysenv@SystemEnv{..} TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=P.ConsumeScriptAddress _ _ _} = True
      isScriptIn _                                                     = False

      scriptInputs = filter isScriptIn (Set.elems txCandidateInputs)

      collectCollaterals knownCollaterals = do
        let
          estimateCollateral' collaterals = do
            (C.BalancedTxBody _ _ fee) <- buildBalancedTx sysenv dummyAddr collaterals txc
            let (C.Quantity fee')  = C.lovelaceToQuantity fee
                collateralPercent' = naturalToInteger collateralPercent
            pure $ P.Lovelace $ collateralPercent' * fee' `div` 100

        collateral <- estimateCollateral' knownCollaterals
        utxos      <- selectUtxos wallet (P.toValue collateral) >>= maybe (throwM FailedToSatisfyCollateral) pure

        let collaterals = Set.fromList $ fmap Sdk.FullCollateralTxIn $ Set.elems utxos

        collateral' <- estimateCollateral' collaterals

        if collateral' > collateral
        then collectCollaterals collaterals
        else pure collaterals

  case (scriptInputs, collateralPolicy) of
    ([], _)    -> pure mempty
    (_, Cover) -> collectCollaterals mempty
    _          -> throwM CollateralNotAllowed

dummyAddr :: Sdk.ChangeAddress
dummyAddr =
  Sdk.ChangeAddress $ P.pubKeyHashAddress $ P.PubKeyHash $ P.BuiltinByteString (B8.pack $ show (0 :: Word64))
