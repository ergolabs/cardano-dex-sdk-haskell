module SubmitAPI.Internal.Transaction where

import           RIO
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map

import           Codec.Serialise           (serialise)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Text.Prettyprint.Doc (Pretty(..))
import qualified Data.Set                  as Set

import           Cardano.Api          hiding (TxBodyError)
import           Cardano.Api.Shelley  (ProtocolParameters(..))
import qualified Ledger               as P
import qualified Ledger.Tx.CardanoAPI as Interop
import qualified Ledger.Ada           as Ada

import qualified CardanoTx.Models   as Sdk
import           CardanoTx.ToPlutus
import           NetworkAPI.Env

signTx
  :: TxBody AlonzoEra
  -> [ShelleyWitnessSigningKey]
  -> Tx AlonzoEra
signTx body keys =
  makeSignedTransaction wits body
    where wits = keys <&> makeShelleyKeyWitness body

buildBalancedTx
  :: (MonadThrow f, MonadIO f)
  => SystemEnv
  -> Sdk.ChangeAddress
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (BalancedTxBody AlonzoEra)
buildBalancedTx SystemEnv{..} defaultChangeAddr collateral txc@Sdk.TxCandidate{..} = do
  let eraInMode    = AlonzoEraInCardanoMode
      witOverrides = Nothing
  txBody     <- buildTxBodyContent pparams network collateral txc
  inputsMap  <- buildInputsUTxO network (Set.elems txCandidateInputs)
  changeAddr <- absorbError $ case txCandidateChangePolicy of
    Just (Sdk.ReturnTo addr) -> Interop.toCardanoAddress network addr
    _                        -> Interop.toCardanoAddress network $ Sdk.getAddress defaultChangeAddr
  absorbBalancingError $ makeTransactionBodyAutoBalance eraInMode sysstart eraHistory pparams pools inputsMap txBody changeAddr witOverrides
    where
      absorbBalancingError (Left e)  = throwM $ BalancingError $ T.pack $ show e
      absorbBalancingError (Right a) = pure a

estimateTxFee
  :: (MonadThrow f, MonadIO f)
  => ProtocolParameters
  -> NetworkId
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f Lovelace
estimateTxFee pparams@ProtocolParameters{..} network collateral txc = do
  txBodyContent <- buildTxBodyContent pparams network collateral txc
  txBody        <- either (throwM . TxBodyError . T.pack . show) pure (makeTransactionBody txBodyContent)
  let noWitTx = makeSignedTransaction [] txBody
  pure $ estimateTransactionFee network protocolParamTxFeeFixed protocolParamTxFeePerByte noWitTx 0 0 0 0

buildTxBodyContent
  :: (MonadThrow f, MonadIO f)
  => ProtocolParameters
  -> NetworkId
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (TxBodyContent BuildTx AlonzoEra)
buildTxBodyContent protocolParams network collateral Sdk.TxCandidate{..} = do
  txIns           <- buildTxIns $ Set.elems txCandidateInputs
  txInsCollateral <- buildTxCollateral $ Set.elems collateral
  txOuts          <- buildTxOuts network txCandidateOutputs
  txFee           <- absorbError $ Interop.toCardanoFee dummyFee
  txValidityRange <- absorbError $ Interop.toCardanoValidityRange txCandidateValidRange
  txMintValue     <-
    let redeemers = buildMintRedeemers txCandidateMintInputs
        valueMint = Sdk.unMintValue txCandidateValueMint
        policies  = Sdk.mintInputsPolicies txCandidateMintInputs
    in absorbError $ Interop.toCardanoMintValue redeemers valueMint policies
  pure $ TxBodyContent
    { txIns             = txIns
    , txInsCollateral   = txInsCollateral
    , txOuts            = txOuts
    , txFee             = txFee
    , txValidityRange   = txValidityRange
    , txMintValue       = txMintValue
    , txProtocolParams  = BuildTxWith $ Just protocolParams
    , txScriptValidity  = TxScriptValidityNone
    -- unused:
    , txMetadata       = TxMetadataNone
    , txAuxScripts     = TxAuxScriptsNone
    , txExtraKeyWits   = TxExtraKeyWitnessesNone
    , txWithdrawals    = TxWithdrawalsNone
    , txCertificates   = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    }

buildTxIns
  :: (MonadThrow f, MonadIO f)
  => [Sdk.FullTxIn]
  -> f [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
buildTxIns =
    mapM translate
  where
    translate Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{..}, ..} = do
      sWit <- absorbError $ Interop.toCardanoTxInWitness fullTxInType
      txIn <- absorbError $ Interop.toCardanoTxIn fullTxOutRef

      pure (txIn, BuildTxWith sWit)

buildTxCollateral
  :: (MonadThrow f, MonadIO f)
  => [Sdk.FullCollateralTxIn]
  -> f (TxInsCollateral AlonzoEra)
buildTxCollateral ins =
    TxInsCollateral CollateralInAlonzoEra <$> mapM translate ins
  where
    translate Sdk.FullCollateralTxIn{fullCollateralTxInTxOut=Sdk.FullTxOut{..}} =
      absorbError $ Interop.toCardanoTxIn fullTxOutRef

buildTxOuts
  :: (MonadThrow f, MonadIO f)
  => NetworkId
  -> [Sdk.TxOutCandidate]
  -> f [TxOut CtxTx AlonzoEra]
buildTxOuts network =
    mapM translate
  where
    translate sdkOut = absorbError $ Interop.toCardanoTxOut network $ toPlutus sdkOut

buildInputsUTxO
  :: (MonadThrow f, MonadIO f)
  => NetworkId
  -> [Sdk.FullTxIn]
  -> f (UTxO AlonzoEra)
buildInputsUTxO network inputs =
    mapM (absorbError . translate) inputs <&> UTxO . Map.fromList
  where
    translate Sdk.FullTxIn{fullTxInTxOut=out@Sdk.FullTxOut{..}} = do
      txIn  <- Interop.toCardanoTxIn fullTxOutRef
      txOut <- Interop.toCardanoTxOut network $ toPlutus out

      pure (txIn, toCtxUTxOTxOut txOut)

buildMintRedeemers :: Sdk.MintInputs -> P.Redeemers
buildMintRedeemers Sdk.MintInputs{..} = Map.fromList $ Map.toList mintInputsRedeemers <&> first (P.RedeemerPtr P.Mint)

collectInputsData :: MonadThrow f => [Sdk.FullTxIn] -> f (Map.Map P.DatumHash P.Datum)
collectInputsData inputs = do
  rawData <- mapM extractInputDatum inputs
  pure $ Map.fromList $ rawData >>= maybe mempty pure

extractInputDatum :: MonadThrow f => Sdk.FullTxIn -> f (Maybe (P.DatumHash, P.Datum))
extractInputDatum Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutDatumHash=Just dh, fullTxOutDatum=Just d}} =
  pure $ Just (dh, d)
extractInputDatum Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutDatumHash=Just dh}} =
  throwM $ UnresolvedData dh
extractInputDatum _ = pure Nothing

dummyFee :: P.Value
dummyFee = Ada.lovelaceValueOf 0

data TxAssemblyError
  = EvaluationError Text
  | TxBodyError Text
  | DeserializationError
  | InvalidValidityRange
  | ValueNotPureAda
  | OutputHasZeroAda
  | StakingPointersNotSupported
  | SimpleScriptsNotSupportedToCardano
  | MissingTxInType
  | MissingMintingPolicy
  | MissingMintingPolicyRedeemer
  | ScriptPurposeNotSupported P.ScriptTag
  | PublicKeyInputsNotSupported
  | UnresolvedData P.DatumHash
  | BalancingError Text
  | CollateralNotAllowed
  | FailedToSatisfyCollateral
  | SignerNotFound P.PubKeyHash
  deriving (Show, Exception)

absorbError :: (MonadThrow f, MonadIO f) => Either Interop.ToCardanoError a -> f a
absorbError (Left err) = do
  liftIO . print $ "The err has occurred: " ++ show err 
  throwM $ adaptInteropError err
absorbError (Right vl) = pure vl

adaptInteropError :: Interop.ToCardanoError -> TxAssemblyError
adaptInteropError err =
    case err of
      Interop.TxBodyError _                      -> TxBodyError renderedErr
      Interop.DeserialisationError               -> DeserializationError
      Interop.InvalidValidityRange               -> InvalidValidityRange
      Interop.ValueNotPureAda                    -> ValueNotPureAda
      Interop.OutputHasZeroAda                   -> OutputHasZeroAda
      Interop.StakingPointersNotSupported        -> StakingPointersNotSupported
      Interop.SimpleScriptsNotSupportedToCardano -> SimpleScriptsNotSupportedToCardano
      Interop.MissingTxInType                    -> MissingTxInType
      Interop.MissingMintingPolicy               -> MissingMintingPolicy
      Interop.MissingMintingPolicyRedeemer       -> MissingMintingPolicyRedeemer
      Interop.ScriptPurposeNotSupported t        -> ScriptPurposeNotSupported t
      Interop.PublicKeyInputsNotSupported        -> PublicKeyInputsNotSupported
      Interop.Tag _ e                            -> adaptInteropError e
  where renderedErr = T.pack $ show $ pretty err

serializePlutusScript :: P.Script -> SerializedScript
serializePlutusScript s = toShort $ toStrict $ serialise s

type SerializedScript = ShortByteString
