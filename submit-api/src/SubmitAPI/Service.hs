module SubmitAPI.Service where

import qualified RIO.List as L
import           RIO
import qualified Data.Set              as Set
import qualified Data.ByteString.Char8 as B8
import           GHC.Natural           (naturalToInteger)
import qualified PlutusTx.AssocMap as Map
import qualified CardanoTx.Models            as Sdk
import qualified Cardano.Api                 as C
import qualified Ledger                      as P
import qualified PlutusTx.Builtins.Internal  as P
import qualified Ledger.Ada                  as P
import qualified Plutus.V1.Ledger.Credential as P
import           Plutus.V1.Ledger.Api (Value(..))
import           SubmitAPI.Config
import           SubmitAPI.Internal.Transaction
import           SubmitAPI.ViaPAB.Transaction as ViaPAB
import           NetworkAPI.Service           hiding (submitTx)
import qualified NetworkAPI.Service           as Network
import           NetworkAPI.Env
import           WalletAPI.Utxos
import           WalletAPI.Vault
import           Plutus.Contract.Wallet
import           Control.Monad.Freer as Eff
import qualified Ledger.Tx.CardanoAPI as Interop
import PlutusTx.AssocMap as Map
import qualified Ledger.Ada as Ada

data Transactions f = Transactions
  { finalizeTx :: Sdk.TxCandidate  -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService
  :: (MonadThrow f, MonadIO f)
  => Network f
  -> Vault f
  -> TxAssemblyConfig
  -> Transactions f
mkSubmitService network wallet conf = Transactions
  { finalizeTx = finalizeTx' network wallet conf
  , submitTx   = Network.submitTx network
  }

--finalizeTxViaPAB
--  :: MonadThrow f
--  => MonadIO f
--  => WalletOutputs f
--  -> Network f
--  -> TxAssemblyConfig
--  -> Sdk.TxCandidate
--  -> f (C.Tx C.AlonzoEra)
--finalizeTxViaPAB wallet Network{getSystemEnv} conf txc = do
--    sysenv@SystemEnv{..} <- getSystemEnv
--    collaterals          <- selectCollaterals wallet sysenv conf txc
--
--    let utx = ViaPAB.mkUnbalancedTx collaterals txc
--        handled = handleTx utx
--
--    runned <- Eff.runM handled
--    let eitherTx = unsafeFromEither runned
--        txBody = unsafeFromEither $ Interop.toCardanoTxBody [] (Just pparams) network eitherTx
--        res = C.makeSignedTransaction [] txBody
--    return res

finalizeTx'
  :: (MonadThrow f, MonadIO f)
  => Network f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' Network{..} wallet@Vault{..} conf@TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  sysenv      <- getSystemEnv
  _ <- liftIO $ print ("sysenv:" ++ (show sysenv))
  collaterals <- selectCollaterals (narrowVault wallet) sysenv conf txc
  _ <- liftIO $ print ("collaterals:" ++ (show collaterals))
  let
    isBalancedTx = amountIn == amountOut
      where
        amountIn =
          foldr (\ txIn acc -> Sdk.fullTxOutValue (Sdk.fullTxInTxOut txIn) <> acc) mempty (Set.elems txCandidateInputs)
        amountOut =
          foldr (\ txOut acc -> Sdk.txOutCandidateValue txOut <> acc) mempty txCandidateOutputs
  _ <- liftIO $ print ("isBalancedTx1:" ++ (show isBalancedTx))
  (C.BalancedTxBody txb _ _) <- case txCandidateChangePolicy of
    Just (Sdk.ReturnTo changeAddr) -> buildBalancedTx sysenv (Sdk.ChangeAddress changeAddr) collaterals txc
    _ | isBalancedTx               -> buildBalancedTx sysenv dummyAddr collaterals txc
  _ <- liftIO $ print ("isBalancedTx2:" ++ (show isBalancedTx))

  let
    requiredSigners = Set.elems txCandidateInputs >>= getPkh
      where
        getPkh Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _}} = [pkh]
        getPkh _                                                                                                = []
  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) requiredSigners
  pure $ signTx txb signers

selectCollaterals
  :: forall f. MonadThrow f
  => MonadIO f
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
selectCollaterals WalletOutputs{selectUtxos, selectUxtosByFilter} SystemEnv{..} TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=P.ConsumeScriptAddress {}} = True
      isScriptIn _                                                    = False

      scriptInputs = RIO.filter isScriptIn (Set.elems txCandidateInputs)

      collectCollaterals knownCollaterals = do
        let
          estimateCollateral' collaterals = do
            fee <- estimateTxFee pparams network collaterals txc
            _ <- liftIO $ print ("fee:" ++ (show fee))
            let (C.Quantity fee')  = C.lovelaceToQuantity fee
                collateralPercent' = naturalToInteger collateralPercent
            pure $ P.Lovelace $ collateralPercent' * fee' `div` 100
        collateral  <- estimateCollateral' knownCollaterals
        utxosM      <- (selectUxtosByFilter containsOnlyAda) :: f (Maybe (Set.Set Sdk.FullTxOut)) -- only for debug
        utxos       <- case utxosM of
          Nothing -> throwM FailedToSatisfyCollateral
          Just a  -> pure a
          -- selectUtxos (P.toValue collateral) >>= maybe (throwM FailedToSatisfyCollateral) pure

        let collaterals = Set.fromList $ Set.elems utxos <&> Sdk.FullCollateralTxIn

        collateral' <- estimateCollateral' collaterals

        if collateral' > collateral
          then collectCollaterals collaterals
          else pure collaterals

  case (scriptInputs, collateralPolicy) of
    ([], _)    -> pure mempty
    (_, Cover) -> collectCollaterals mempty  
    _          -> throwM CollateralNotAllowed

containsOnlyAda :: Sdk.FullTxOut -> Bool
containsOnlyAda Sdk.FullTxOut{..} = 
  let
    checkedValue            = Map.toList $ getValue fullTxOutValue
    currencySymbolCondition = L.length checkedValue == 1
    tokenNameConditione     = case L.headMaybe checkedValue of
                                Just (_, tns) -> L.length (Map.toList tns) == 1
                                _             -> False
  in currencySymbolCondition && tokenNameConditione


dummyAddr :: Sdk.ChangeAddress
dummyAddr =
  Sdk.ChangeAddress $ P.pubKeyHashAddress (P.PaymentPubKeyHash $ P.PubKeyHash $ P.BuiltinByteString (B8.pack $ show (0 :: Word64))) Nothing
