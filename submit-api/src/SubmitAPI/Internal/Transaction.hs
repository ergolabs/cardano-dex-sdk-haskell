{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module SubmitAPI.Internal.Transaction where

import           RIO
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map
import qualified Cardano.Api         as Crypto
import qualified Data.ByteString.Base16  as Hex
import qualified Debug.Trace
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Data.Text.Encoding     as T

import           Plutus.V1.Ledger.Address  (toPubKeyHash)
import           Data.Maybe.Strict
import           Cardano.Ledger.Crypto     (StandardCrypto)
import           Codec.Serialise           (serialise)
import           Cardano.Ledger.Alonzo.PlutusScriptApi
import           Cardano.Ledger.Alonzo.Scripts (Script (TimelockScript, PlutusScript))
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as LAPS
import qualified Cardano.Ledger.Alonzo as LA
import           Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), unRedeemers, unTxDats)
import           Ledger.Address
import qualified Data.Array as Array
import Codec.CBOR.Encoding (Encoding)
import qualified Cardano.Ledger.Alonzo
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import           Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tools as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import           ErgoDex.Amm.PoolActions
import Cardano.Ledger.TxIn (TxIn (..), txid)
import           Cardano.Ledger.Alonzo.TxWitness (TxWitness (..), txscripts', txdats, unTxDats)
import qualified Cardano.Ledger.Core as Core
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.Except (runExcept)
import           GHC.Records (HasField (..))
import           CardanoTx.Address
import qualified Codec.Serialise  as Codec
import           Cardano.Ledger.Alonzo.Data (getPlutusData)
import           Common.Throw.Combinators
import           Data.ByteString.Lazy      (toStrict)
import qualified Data.Bifunctor as Bi
import qualified Plutus.V1.Ledger.Api  as Api
import           Data.Text.Prettyprint.Doc (Pretty(..))
import qualified Data.ByteString.Short as SBS
import qualified Cardano.Ledger.Core       as Core
import qualified Data.Set                  as Set
import Cardano.Slotting.Time (SystemStart)
import qualified Cardano.Ledger.Alonzo.TxBody as ATx
import           Cardano.Ledger.Alonzo.TxInfo
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as LE
import qualified Cardano.Ledger.Hashes as LH
import qualified Cardano.Ledger.Alonzo.Tx  as AT
import           Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)

import qualified Cardano.Api  as C
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Api.Byron as C
import qualified Cardano.Ledger.Shelley.UTxO  as Shelley
import qualified Cardano.Api.Shelley  as C
import           Cardano.Api          hiding (TxBodyError, TxIn(..))
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Cardano.Ledger.Alonzo.TxWitness as C
import           Cardano.Api.Shelley  (ProtocolParameters(..), ShelleyLedgerEra)
import qualified Ledger               as P
import qualified Ledger.Tx.CardanoAPI as Interop
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded)
import qualified Ledger.Ada           as Ada

import qualified CardanoTx.Models   as Sdk
import           CardanoTx.ToPlutus
import           GHC.Records (HasField (getField))
import           NetworkAPI.Env

signTx
  :: TxBody AlonzoEra
  -> [ShelleyWitnessSigningKey]
  -> Tx AlonzoEra
signTx body keys =
  makeSignedTransaction wits body
    where wits = keys <&> makeShelleyKeyWitness body

instance Exception Interop.ToCardanoError
instance Exception TransactionValidityIntervalError

unsafeFromEitherTest :: Either a b -> b
unsafeFromEitherTest (Right b) = b
unsafeFromEitherTest (Left a)  = Prelude.error "unsafeFromEitherTest"

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
  let cp = map Sdk.mkPkhTxIn (map Sdk.fullCollateralTxInTxOut (Set.elems collateral))
  _ <- liftIO $ print ("inputsMap: " ++ (show (Set.elems (txCandidateInputs))))
  inputsMap  <- buildInputsUTxO network (Set.elems (txCandidateInputs))
  inputsAlonzoMap <- buildInputsUTxOMap network (Set.elems (txCandidateInputs))
  _ <- liftIO $ print ("inputsMap: " ++ (show inputsMap))
  _ <- liftIO $ print ("txBody: " ++ (show txBody))
  _ <- liftIO $ print ("collateral: " ++ (show collateral))
  _ <- liftIO $ print ("txCandidateChangePolicy: " ++ (show txCandidateChangePolicy))
  _ <- liftIO $ case txCandidateChangePolicy of
                    Just (Sdk.ReturnTo addr) -> print ("addr1:" ++ (show (Interop.toCardanoAddress network addr)))
                    _                        -> print ("addr2:" ++ (show (Interop.toCardanoAddress network $ Sdk.getAddress defaultChangeAddr)))
  changeAddr <- absorbError $ case txCandidateChangePolicy of
    Just (Sdk.ReturnTo addr) -> Interop.toCardanoAddress network addr
    _                        -> Interop.toCardanoAddress network $ Sdk.getAddress defaultChangeAddr
  _ <- liftIO $ print ("changeAddr: " ++ (show changeAddr))
  _ <- liftIO $ print ("eraInMode: " ++ (show eraInMode))
  _ <- liftIO $ print ("sysstart: " ++ (show sysstart))
  _ <- liftIO $ print ("eraHistory: " ++ (show eraHistory))
  _ <- liftIO $ print ("pparams: " ++ (show pparams))
  _ <- liftIO $ print ("pools: " ++ (show pools))
  _ <- liftIO $ print ("inputsMap: " ++ (show inputsMap))
  _ <- liftIO $ print ("changeAddr: " ++ (show changeAddr))
  let
    executorAddrM = readShellyAddress "addr_test1qrt56fk9q2w09yqffl8p5pnsmfeknwgzv4calwthcmazxnl0pwy4t9rk9qlzhd49k40p0yxrsm5c5f8puxlxc9hrqj4sy2tu8f"
  executorAddr <- throwMaybe (PriceTooHigh) executorAddrM
  _ <- liftIO $ print ("execuror addr: " ++ (show executorAddr))
  let
    pubKeyM = toPubKeyHash executorAddr
  pubKey <- throwMaybe (PriceTooHigh) pubKeyM
  let
    ppkh = PaymentPubKeyHash pubKey
    cardanoHashM = Interop.toCardanoPaymentKeyHash ppkh
  cardanoHash <- throwEither cardanoHashM
  _ <- liftIO $ print ("pubKey: " ++ (show pubKey))
  _ <- do
    body <- either (throwM . TxBodyError . T.pack . show) pure (makeTransactionBody txBody)
    _ <- liftIO $ print ("txBody: " ++ (show body))
    let
      keyM = decryptKey
    key <- throwMaybe (PriceTooHigh) keyM
    let
      wK = WitnessPaymentKey key
      witness2put = makeShelleyKeyWitness body wK
      tx@(C.ShelleyTx ShelleyBasedEraAlonzo txv) = makeSignedTransaction [witness2put] body :: Tx AlonzoEra
      ws = getTxWitnesses tx
      txBodyA  = getTxBody tx :: TxBody AlonzoEra
      scripts = Interop.plutusScriptsFromTxBody txBodyA
      -- test = evaluateTransactionExecutionUnits eraInMode sysstart eraHistory pparams inputsMap txBodyA
      tvBody = AT.body txv
      wits@(TxWitness _ _ sc _ _) = AT.wits txv
      da@(TxWitness _ _ _ dats ra) = AT.wits txv
      scriptsW = txscripts' (AT.wits txv)
      ei       = (toLedgerEpochInfo eraHistory)
      utxoInEra = (toLedgerUTxO ShelleyBasedEraAlonzo inputsMap) :: Shelley.UTxO (LA.AlonzoEra StandardCrypto)
      list = (testCheck txv utxoInEra)
      testMap = Map.fromList $ do
        (sp, sh) <- list
        msb <- case Map.lookup sh scriptsW of
          Nothing -> pure Nothing
          Just (TimelockScript _) -> []
          Just (Cardano.Ledger.Alonzo.Scripts.PlutusScript v bytes) -> pure $ Just (bytes, v)
        pointer <- case AT.rdptr @(LA.AlonzoEra StandardCrypto) tvBody sp of
          SNothing -> []
          -- Since scriptsNeeded used the transaction to create script purposes,
          -- it would be a logic error if rdptr was not able to find sp.
          SJust p -> pure p
        pure (pointer, (sp, msb))
      alonzoCost = (toAlonzoCostModels (protocolParamCostModels pparams)) :: Array.Array Alonzo.Language Alonzo.CostModel
      alonzoParams = (toLedgerPParams ShelleyBasedEraAlonzo pparams)
      getInfo lang = (,) lang <$> getTestInfo alonzoParams lang ei sysstart utxoInEra txv
    txInfos <- throwEither $ Array.array (Alonzo.PlutusV1, Alonzo.PlutusV2) <$> mapM getInfo (Set.toList Alonzo.nonNativeLanguages)
    let
      b = do
        txInfos <- Array.array (Alonzo.PlutusV1, Alonzo.PlutusV2) <$> mapM getInfo (Set.toList Alonzo.nonNativeLanguages)
        pure $ Map.mapWithKey (findAndCount alonzoParams txInfos) (C.unRedeemers $ ra)
        where
          findAndCount ::
            Core.PParams (LA.AlonzoEra StandardCrypto) ->
            Array.Array Alonzo.Language VersionedTxInfo ->
            RdmrPtr ->
            (Alonzo.Data (LA.AlonzoEra StandardCrypto), Alonzo.ExUnits) ->
            Either (Alonzo.ScriptFailure StandardCrypto) Alonzo.ExUnits
          findAndCount pparams info pointer (rdmr, _) = do
            (sp, mscript) <- note (Alonzo.RedeemerNotNeeded pointer) $ Map.lookup pointer testMap
            (script, lang) <- note (Alonzo.MissingScript pointer) mscript
            let inf = info Array.! lang
            let (l1, l2) = Array.bounds alonzoCost
            (Alonzo.CostModel costModel) <-
              if l1 <= lang && lang <= l2 then Right (alonzoCost Array.! lang) else Left (Alonzo.NoCostModel lang)
            args <- case sp of
              (AT.Spending txin) -> do
                txOut <- note (Alonzo.UnknownTxIn txin) $ Map.lookup txin (inputsAlonzoMap)
                let mdh = getHash $ C.toShelleyTxOut ShelleyBasedEraAlonzo (C.toCtxUTxOTxOut txOut)
                --let TxOut _ _ mdh = txOut
                dh <- note (Alonzo.InvalidTxIn txin) $ strictMaybeToMaybe mdh
                dat <- note (Alonzo.MissingDatum dh) $ Map.lookup dh (unTxDats dats)
                pure [dat, rdmr, valContext inf sp]
              _ -> pure [rdmr, valContext inf sp]
            let
              pArgs = map getPlutusData args
              datum = pArgs!!0
              redeemer = pArgs!!1
              ctx = pArgs!!2
              encodedD = Codec.encode datum
              encodedR = Codec.encode redeemer
              encoded = Codec.encode ctx
              scriptSbs = LE.decodeUtf8 . Hex.encode .SBS.fromShort $ script
              a = trace (T.pack ("maxBudget: " ++ show (maxBudget))) lang
            case interpreter lang costModel maxBudget script pArgs of
              (logs, Left e) -> case lang of
                Alonzo.PlutusV1 -> Left $ Alonzo.ValidationFailedV1 e [T.pack ("maxBudget1: " ++ show maxBudget ++ ". logs: " ++ show logs ++ ". e: " ++ show e ++ ". encoded:" ++ (show (serializeEncoding encoded))++ show e ++ ". encodedD:" ++ (show (serializeEncoding encodedD))++ show e ++ ". encodedR:" ++ (show (serializeEncoding encodedR)) ++ ". script: " ++ (show scriptSbs))]
                Alonzo.PlutusV2 -> Left $ Alonzo.ValidationFailedV2 e [T.pack ("maxBudget2: " ++ show maxBudget ++ ". logs: " ++ show logs ++ ". e: " ++ show e)]
              (_, Right exBudget) -> note (Alonzo.ValidationFailedV1 Api.CostModelParameterMismatch [T.pack ("maxBudget3: " ++ show maxBudget)]) $ exBudgetToExUnits exBudget
            where
              maxBudget = transExUnits . _maxTxExUnits $ pparams
              interpreter lang = case lang of
                Alonzo.PlutusV1 -> Api.evaluateScriptRestricting Api.Verbose
                Alonzo.PlutusV2 -> Api.evaluateScriptRestricting Api.Verbose
    let
--    _ <- liftIO $ print ("tx: " ++ (show tx))
--    _ <- liftIO $ print ("wits: " ++ (show wits))
--    _ <- liftIO $ print ("scriptsW: " ++ (show scriptsW))
--    _ <- liftIO $ print ("scripts: " ++ (show scripts))
--    _ <- liftIO $ print ("scriptsNeeded: " ++ (show list))
--    _ <- liftIO $ print ("testMap: " ++ (show testMap))
    _ <- liftIO $ print ("ra: " ++ (show ra))
    _ <- liftIO $ print ("testMap: " ++ (show testMap))
    _ <- liftIO $ print ("alonzoCost: " ++ (show (Alonzo.PlutusV1, Alonzo.PlutusV2)))
--    _ <- liftIO $ print ("txInfos p1: " ++ (show (txInfos Array.! Alonzo.PlutusV1)))
--    _ <- liftIO $ print ("txInfos p2: " ++ (show (txInfos Array.! Alonzo.PlutusV1)))
--    _ <- liftIO $ print ("alonzoCost p1: " ++ (show (alonzoCost Array.! Alonzo.PlutusV1)))
--    _ <- liftIO $ print ("alonzoCost p2: " ++ (show (alonzoCost Array.! Alonzo.PlutusV1)))
    _ <- liftIO $ print ("alonzoParams: " ++ (show $ transExUnits . _maxTxExUnits $ alonzoParams))
    -- _ <- liftIO $ print ("body: " ++ (show txBodyA))
--    _ <- liftIO $ print ("witness: " ++ (show ws))
--    _ <- liftIO $ print ("utxo: " ++ (show inputsMap))
    _ <- liftIO $ print ("testF: " ++ (show b))
    -- _ <- liftIO $ print ("test: " ++ (show test))
    pure ()
  absorbBalancingError $ makeTransactionBodyAutoBalance eraInMode sysstart eraHistory pparams pools inputsMap txBody changeAddr witOverrides
    where
      absorbBalancingError (Left e)  = do
        _ <- liftIO $ print ("error: " ++ (show e))
        throwM $ BalancingError $ T.pack $ show e
      absorbBalancingError (Right a) = pure a
--
--toLedgerUTxO1 (UTxO utxo) =
--  Map.fromList
--  . map (Bi.bimap C.toShelleyTxIn (C.fromShelleyTxOut ShelleyBasedEraAlonzo value))
--  . Map.toList
--  $ utxo
--
--toLedgerUTxO2 (UTxO utxo) =
--  Shelley.UTxO
--  . Map.fromList
--  . map (Bi.bimap C.toShelleyTxIn (C.fromShelleyTxOut ShelleyBasedEraAlonzo value))
--  . Map.toList
--  $ utxo

serializeEncoding :: Encoding -> Text
serializeEncoding =
  LE.decodeUtf8 . Hex.encode . BSL.toStrict . Builder.toLazyByteStringWith strategy mempty . CBOR.Write.toBuilder
  where
    -- 1024 is the size of the first buffer, 4096 is the size of subsequent
    -- buffers. Chosen because they seem to give good performance. They are not
    -- sacred.
        strategy = Builder.safeStrategy 1024 4096

getHash (ATx.TxOutCompact _ _) = SNothing
getHash (ATx.TxOutCompactDH _ _ d) = SJust d

toAlonzoCostModels :: Map AnyPlutusScriptVersion CostModel
                   -> Array.Array Alonzo.Language Alonzo.CostModel
toAlonzoCostModels costmodels =
  Array.array
    (minBound, maxBound)
    [ (toAlonzoLanguage lang, toAlonzoCostModel costmodel)
    | (lang, costmodel) <- Map.toList costmodels ]

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

toAlonzoLanguage :: AnyPlutusScriptVersion -> Alonzo.Language
toAlonzoLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Alonzo.PlutusV1
toAlonzoLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Alonzo.PlutusV2

toAlonzoCostModel :: CostModel -> Alonzo.CostModel
toAlonzoCostModel (CostModel m) = Alonzo.CostModel m

toLedgerEpochInfo :: EraHistory mode -> EpochInfo (Either TransactionValidityIntervalError)
toLedgerEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first TransactionValidityIntervalError . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

testCheck ::
  Core.Tx (LA.AlonzoEra StandardCrypto) ->
    -- | The current UTxO set (or the relevant portion for the transaction).
  Shelley.UTxO (LA.AlonzoEra StandardCrypto) ->
  [(AT.ScriptPurpose (Crypto (LA.AlonzoEra StandardCrypto)), LH.ScriptHash (Crypto (LA.AlonzoEra StandardCrypto)))]
testCheck tx utxo = scriptsNeeded utxo tx

getTestInfo ::
  Core.PParams (LA.AlonzoEra StandardCrypto) ->
  Alonzo.Language ->
  EpochInfo (Either TransactionValidityIntervalError) ->
  SystemStart ->
  Shelley.UTxO (LA.AlonzoEra StandardCrypto) ->
  AT.ValidatedTx (LA.AlonzoEra StandardCrypto) ->
  (Either TransactionValidityIntervalError) VersionedTxInfo
getTestInfo = txInfo

decryptKey :: Maybe (Crypto.SigningKey Crypto.PaymentKey)
decryptKey = --do
  Just $ unsafeFromEitherTest $ Crypto.deserialiseFromCBOR asSK (unsafeFromEitherTest $ Hex.decode . T.encodeUtf8 $ "582075bcd3df982e1bc89bdf261c0ccda780cc64be3ccd3cb84dcb1822573ab643ed")
      where asSK = Crypto.AsSigningKey Crypto.AsPaymentKey

estimateTxFee
  :: (MonadThrow f, MonadIO f)
  => ProtocolParameters
  -> NetworkId
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f Lovelace
estimateTxFee pparams network collateral txc = do
  txBodyContent <- buildTxBodyContent pparams network collateral txc
  txBody        <- either (throwM . TxBodyError . T.pack . show) pure (makeTransactionBody txBodyContent)
  pure $ evaluateTransactionFee pparams txBody 0 0

buildTxBodyContent
  :: (MonadThrow f, MonadIO f)
  => ProtocolParameters
  -> NetworkId
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (TxBodyContent BuildTx AlonzoEra)
buildTxBodyContent protocolParams network collateral Sdk.TxCandidate{..} = do
  _ <- liftIO $ print ("buildTxBodyContent")
  txIns           <- buildTxIns $ Set.elems txCandidateInputs
  _ <- liftIO $ print ("txIns: " ++ (show txIns))
  txInsCollateral <- buildTxCollateral $ Set.elems collateral
  _ <- liftIO $ print ("txInsCollateral: " ++ (show txInsCollateral))
  txOuts          <- buildTxOuts network txCandidateOutputs
  _ <- liftIO $ print ("txOuts: " ++ (show txOuts))
  txFee           <- absorbError $ Interop.toCardanoFee dummyFee
  _ <- liftIO $ print ("txFee: " ++ (show txFee))
  txValidityRange <- absorbError $ Interop.toCardanoValidityRange txCandidateValidRange
  _ <- liftIO $ print ("txValidityRange: " ++ (show txValidityRange))
  txMintValue     <-
    let redeemers = buildMintRedeemers txCandidateMintInputs
        valueMint = Sdk.unMintValue txCandidateValueMint
        policies  = Sdk.mintInputsPolicies txCandidateMintInputs
    in absorbError $ Interop.toCardanoMintValue redeemers valueMint policies
  let
    executorAddrM = readShellyAddress "addr_test1qrt56fk9q2w09yqffl8p5pnsmfeknwgzv4calwthcmazxnl0pwy4t9rk9qlzhd49k40p0yxrsm5c5f8puxlxc9hrqj4sy2tu8f"
  executorAddr <- throwMaybe (PriceTooHigh) executorAddrM
  _ <- liftIO $ print ("execuror addr: " ++ (show executorAddr))
  let
    pubKeyM = toPubKeyHash executorAddr
  pubKey <- throwMaybe (PriceTooHigh) pubKeyM
  let
    ppkh = PaymentPubKeyHash pubKey
    cardanoHashM = Interop.toCardanoPaymentKeyHash ppkh
  cardanoHash <- throwEither cardanoHashM
  _ <- liftIO $ print ("pubKey: " ++ (show pubKey))
  _ <- liftIO $ print ("txBody: " ++ (show txMintValue))
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
  -> f [(C.TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
buildTxIns =
    mapM translate
  where
    translate Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{..}, ..} = do
      sWit <- absorbError $ toCardanoTxInWitness fullTxInType
      txIn <- absorbError $ Interop.toCardanoTxIn fullTxOutRef

      pure (txIn, BuildTxWith sWit)

toCardanoTxInWitness :: P.TxInType -> Either Interop.ToCardanoError (C.Witness C.WitCtxTxIn C.AlonzoEra)
toCardanoTxInWitness P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness P.ConsumeSimpleScriptAddress = Left Interop.SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness
    (P.ConsumeScriptAddress
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
        <$> toCardanoPlutusScript validator
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure (C.ExecutionUnits 2000 2000)
        )

toCardanoPlutusScript :: P.Script -> Either Interop.ToCardanoError (C.PlutusScript C.PlutusScriptV1)
toCardanoPlutusScript = deserialiseFromRawBytes1 (C.AsPlutusScript C.AsPlutusScriptV1) . BSL.toStrict . Codec.serialise

toCardanoScriptData :: Api.BuiltinData -> C.ScriptData
toCardanoScriptData = C.fromPlutusData . Api.builtinDataToData

deserialiseFromRawBytes1 :: C.SerialiseAsRawBytes t => C.AsType t -> ByteString -> Either Interop.ToCardanoError t
deserialiseFromRawBytes1 asType = maybe (Left Interop.DeserialisationError) Right . C.deserialiseFromRawBytes asType

buildTxCollateral
  :: (MonadThrow f, MonadIO f)
  => [Sdk.FullCollateralTxIn]
  -> f (TxInsCollateral AlonzoEra)
buildTxCollateral ins =
    TxInsCollateral CollateralInAlonzoEra <$> mapM translate ins
  where
    translate Sdk.FullCollateralTxIn{fullCollateralTxInTxOut=Sdk.FullTxOut{..}} =
      absorbError $ Interop.toCardanoTxIn fullTxOutRef

--todo: do refactoring
buildTxOuts
  :: (MonadThrow f, MonadIO f)
  => NetworkId
  -> [Sdk.TxOutCandidate]
  -> f [TxOut CtxTx AlonzoEra]
buildTxOuts network =
    mapM translate
  where
    translate sdkOut = do
      let
        plutusOut = toPlutus sdkOut
        tm = do
          dh <- fmap P.datumHash (Sdk.txOutCandidateDatum sdkOut)
          d <- Sdk.txOutCandidateDatum sdkOut
          return (Map.singleton dh d)
        datumMap = case tm of
          Just a -> a
          Nothing -> Map.empty

      absorbError $ Interop.toCardanoTxOut network datumMap plutusOut

buildInputsUTxOMap
  :: (MonadThrow f, MonadIO f)
  => NetworkId
  -> [Sdk.FullTxIn]
  -> f (Map (TxIn StandardCrypto) (TxOut CtxTx AlonzoEra))
buildInputsUTxOMap network inputs = do
  toUtxoInputs <- mapM (absorbError . translate) inputs
  return $ Map.fromList $ toUtxoInputs
  where
    translate Sdk.FullTxIn{fullTxInTxOut=out@Sdk.FullTxOut{..}} = do
      txE <- Interop.toCardanoTxIn fullTxOutRef
      let
        txIn = C.toShelleyTxIn txE
      let
        tm = do
          dh <- fullTxOutDatumHash
          d  <- fullTxOutDatum
          return (Map.singleton dh d)
        datumMap = case tm of
          Just a -> a
          Nothing -> Map.empty
      txOut <- Interop.toCardanoTxOut network datumMap (toPlutus out)
      pure (txIn, txOut)

buildInputsUTxO
  :: (MonadThrow f, MonadIO f)
  => NetworkId
  -> [Sdk.FullTxIn]
  -> f (UTxO AlonzoEra)
buildInputsUTxO network inputs = do
  toUtxoInputs <- mapM (absorbError . translate) inputs
  return $ UTxO . Map.fromList $ toUtxoInputs
  where
    translate Sdk.FullTxIn{fullTxInTxOut=out@Sdk.FullTxOut{..}} = do
      txIn  <- Interop.toCardanoTxIn fullTxOutRef
      let
        tm = do
          dh <- fullTxOutDatumHash
          d  <- fullTxOutDatum
          return (Map.singleton dh d)
        datumMap = case tm of
          Just a -> a
          Nothing -> Map.empty
      txOut <- Interop.toCardanoTxOut network datumMap (toPlutus out)
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
      --todo: check Interop.PublicKeyInputsNotSupported        -> PublicKeyInputsNotSupported
      Interop.Tag _ e                            -> adaptInteropError e
  where renderedErr = T.pack $ show $ pretty err

serializePlutusScript :: P.Script -> SerializedScript
serializePlutusScript s = toShort $ toStrict $ serialise s

type SerializedScript = ShortByteString
