module SubmitAPI.Service where

import qualified RIO.List as L
import           RIO
import qualified Data.Set              as Set
import qualified Data.ByteString.Char8 as B8
import           GHC.Natural           (naturalToInteger)

import qualified PlutusTx.AssocMap           as Map
import qualified Cardano.Api                 as C
import qualified Ledger                      as P
import qualified PlutusTx.Builtins.Internal  as P
import qualified Ledger.Ada                  as P
import qualified Plutus.V1.Ledger.Credential as P
import           Plutus.V1.Ledger.Api        (Value(..))

import qualified CardanoTx.Models               as Sdk
import           SubmitAPI.Config
import           SubmitAPI.Internal.Transaction
import           NetworkAPI.Service             hiding (submitTx)
import qualified NetworkAPI.Service             as Network
import           NetworkAPI.Env
import           WalletAPI.Utxos
import           WalletAPI.Vault

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

--wallet3 only for test
pPubKeyHashReward :: P.PubKeyHash
pPubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

finalizeTx'
  :: MonadThrow f
  => MonadIO f
  => Network f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' Network{..} wallet@Vault{..} conf@TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  sysenv      <- getSystemEnv
  collaterals <- selectCollaterals (narrowVault wallet) sysenv conf txc
  _ <- liftIO $ print ("collaterals:" ++ (show collaterals))
  let
    isBalancedTx = amountIn == amountOut
      where
        amountIn =
          foldr (\ txIn acc -> Sdk.fullTxOutValue (Sdk.fullTxInTxOut txIn) <> acc) mempty (Set.elems txCandidateInputs)
        amountOut =
          foldr (\ txOut acc -> Sdk.txOutCandidateValue txOut <> acc) mempty txCandidateOutputs
  _ <- liftIO $ print ("isBalancedTx:" ++ (show isBalancedTx))
  (C.BalancedTxBody txb _ _) <- case txCandidateChangePolicy of
    Just (Sdk.ReturnTo changeAddr) -> buildBalancedTx sysenv (Sdk.ChangeAddress changeAddr) collaterals txc
    _ | isBalancedTx               -> buildBalancedTx sysenv dummyAddr collaterals txc

  let
    requiredSigners = Set.elems txCandidateInputs >>= getPkh
      where
        -- getPkh Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _}} = [pkh] || only for test
        getPkh _                                                                                                = [pPubKeyHashReward]
  _ <- liftIO $ print ("txb:" ++ (show txb))
  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) requiredSigners
  pure $ signTx txb signers

updateRedeemers :: TxBody AlonzoEra -> TxBody AlonzoEra
updateRedeemers (ShelleyTxBody a txBody b c d e) =
  let body' = body
    { Alonzo.reqSignerHashes = Set.fromList $ hashKey <$> vks
    }

selectCollaterals
  :: MonadThrow f
  => MonadIO f
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
selectCollaterals WalletOutputs{selectUtxos, selectUxtosByFilter} SystemEnv{..} TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=P.ConsumeScriptAddress {}} = True
      isScriptIn _                                                    = False

      scriptInputs = filter isScriptIn (Set.elems txCandidateInputs)

      collectCollaterals knownCollaterals = do
        let
          estimateCollateral' collaterals = do
            fee <- estimateTxFee pparams network collaterals txc
            _   <- liftIO $ (print $ "fee: " ++ (show fee))
            let (C.Quantity fee')  = C.lovelaceToQuantity fee
                collateralPercent' = naturalToInteger collateralPercent
            pure $ P.Lovelace $ collateralPercent' * fee' `div` 100

        collateral  <- estimateCollateral' knownCollaterals
        utxosM      <- selectUxtosByFilter containsOnlyAda -- only for test
        utxos       <- case utxosM of
          Nothing -> throwM FailedToSatisfyCollateral
          Just a  -> pure . Set.singleton . head $ Set.elems a
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
