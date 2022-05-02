module SubmitCli.SubmitCli
  ( SubmitCliService(..)
  , SKeyPath(..)
  , makeSubmitCliService
  ) where

import qualified Ledger.Ada as Ada
import qualified Cardano.Api as C
import qualified PlutusTx as PlutusTx
import qualified Ledger as Ledger
import           Plutus.V1.Ledger.Value
import qualified Plutus.V1.Ledger.Address as PAddress
import           PlutusTx.Builtins.Internal 
import           Cardano.Api
import           Cardano.Api.Shelley

import           CardanoTx.Models
import qualified CardanoTx.Models as Sdk
import           SubmitAPI.Config
import           CardanoTx.Address
import           SubmitAPI.Service
import           NetworkAPI.Env
import           WalletAPI.Utxos

import           RIO
import           System.Process
import qualified Data.ByteString.Base16 as Hex
import qualified Data.Text.Encoding     as E
import qualified Data.ByteString.Char8  as BSS
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Data.Aeson             as Encode
import           Codec.Serialise (serialise)

newtype SKeyPath = SKeyPath { unPath :: String }

data SubmitCliService f =
  SubmitCliService
    { submit :: Sdk.TxCandidate -> f ()
    }

makeSubmitCliService 
  :: (MonadIO f, MonadThrow f) 
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> SKeyPath 
  -> DefaultChangeAddress 
  -> SubmitCliService f
makeSubmitCliService wallet env cfg path addr =
  SubmitCliService
    { submit = submit' wallet env cfg path addr
    }

submit' 
  :: (MonadIO f, MonadThrow f) 
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> SKeyPath 
  -> DefaultChangeAddress 
  -> Sdk.TxCandidate 
  -> f ()
submit' wallet env cfg path addr candidate = do
  buildTx' <- buildTx wallet env cfg path addr candidate
  liftIO $ callCommand buildTx' 
  liftIO $ callCommand $ signTx' path
  liftIO $ callCommand submitTx'

buildTx 
  :: (MonadIO f, MonadThrow f) 
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> SKeyPath 
  -> DefaultChangeAddress 
  -> Sdk.TxCandidate 
  -> f String
buildTx wallet env cfg path addr c@Sdk.TxCandidate{..} = do
  collaterals <- selectCollaterals wallet env cfg c
  inputs      <- renderInputs $ toList txCandidateInputs
  return $ mconcat 
    [ "$CARDANO_CLI transaction build"
    , inputs
    , renderOutputs txCandidateOutputs
    , " "
    , "--change-address=" <> changeAddressToCli addr txCandidateChangePolicy
    , " "
    , renderCollaterals $ toList collaterals
    , " "
    , "--testnet-magic ${TESTNET_MAGIC_NUM}"
    , " "
    , "--out-file tx.build"
    , " "
    , "--required-signer " <> (unPath path)
    , " "
    , "--alonzo-era"
    ]

signTx' :: SKeyPath -> String
signTx' path =
  mconcat
    [ "$CARDANO_CLI transaction sign"
    , " "
    , "--tx-body-file tx.build"
    , " "
    , "--signing-key-file " <> (unPath path)
    , " "
    , "--out-file tx.signed"
    ]

submitTx' :: String
submitTx' =
  mconcat
    [ "$CARDANO_CLI transaction submit"
    , " "
    , "--tx-file tx.signed"
    , " "
    , "--testnet-magic $TESTNET_MAGIC_NUM"
    ]

renderCollaterals :: [Sdk.FullCollateralTxIn] -> String
renderCollaterals collaterals =
  foldl (\acc Sdk.FullCollateralTxIn{fullCollateralTxInTxOut=Sdk.FullTxOut{fullTxOutRef}} -> acc <> " --tx-in-collateral " <> renderTxOutRef fullTxOutRef) mempty collaterals

renderInputs :: (MonadIO f) => [Sdk.FullTxIn] -> f String
renderInputs inputs = do
  let
    zipped = zip [0..] inputs
  foldM renderInput mempty zipped

renderInput :: (MonadIO f) => String -> (Integer, Sdk.FullTxIn) -> f String
renderInput acc (index, Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutRef}, fullTxInType}) = do
  let txIn = acc <> " --tx-in " <> renderTxOutRef fullTxOutRef
  txInType <- renderTxInType index fullTxInType
  return $ txIn <> txInType

renderOutputs :: [Sdk.TxOutCandidate] -> String
renderOutputs = foldl (\acc out -> acc <> renderOutput out) mempty 

renderOutput :: Sdk.TxOutCandidate -> String
renderOutput candidate =
  let
    lovelace          = Ada.getLovelace . Ada.fromValue $ Sdk.txOutCandidateValue candidate
    tokens            = flattenValue $ (Sdk.txOutCandidateValue candidate) <> (Ada.lovelaceValueOf (-lovelace))
    prettyTokens      = foldl (\acc (_, _, i) -> mconcat [ acc, "+", show i]) mempty tokens
    prettyFullTokens  = foldl prittyTokenFullValue mempty tokens
    serialisedAddress = unpackAddress (Sdk.txOutCandidateAddress candidate)
    dh                = renderDatumHash (txOutCandidateDatum candidate)
  in " --tx-out " <> serialisedAddress <> "+" <> (show lovelace) <> prettyTokens <> prettyFullTokens <> dh

renderTxInType :: (MonadIO f) => Integer -> Ledger.TxInType -> f String
renderTxInType count (Ledger.ConsumeScriptAddress v r d) = do
  let 
    contractName   = "contract" <> show count <> ".plutus"
    renderContract = " --tx-in-script-file " <> contractName
    script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript $ v :: PlutusScript PlutusScriptV1
  _ <- liftIO $ void $ writeFileTextEnvelope contractName Nothing script

  let 
    redeemerName   = "redeemer" <> show count <> ".json"
    renderRedeemr  = " --tx-in-redeemer-file " <> redeemerName
    redeemerToFile = BSS.unpack $ LBS.toStrict $ Encode.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData r))
  _ <- liftIO $ writeFile redeemerName redeemerToFile

  let
    datumName = "datum" <> show count <> ".json"
    renderDatum = " --tx-in-datum-file " <> datumName
    datumToFile = BSS.unpack $ LBS.toStrict $ Encode.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData d))
  _ <- liftIO $ writeFile datumName datumToFile

  pure $ renderContract <> renderRedeemr <> renderDatum

renderTxInType _ _ = return mempty
    
renderDatumHash :: OutDatum -> String
renderDatumHash out =
  case outDatumHash out of
    Just hash -> " --tx-out-datum-hash=" <> show hash
    _ -> "" 

unpackAddress :: PAddress.Address -> String
unpackAddress address =
    case (fmap (\shelly -> T.unpack $ serialiseAddress shelly) toShellyAddr) of
        Just r -> r
        _      -> ""
  where
    toShellyAddr = renderToShelleyFromAddress (C.Testnet $ C.NetworkMagic 1097911063) address

changeAddressToCli :: DefaultChangeAddress -> Maybe ChangePolicy -> String
changeAddressToCli deafultChangeAddr policy =
  case policy of
    Just (ReturnTo address) -> unpackAddress address
    _                       -> unpackAddress $ unwrapChangeAddress deafultChangeAddr

renderTxOutRef :: Ledger.TxOutRef -> String
renderTxOutRef Ledger.TxOutRef{..} = show txOutRefId ++ "#" ++ (show txOutRefIdx)

prittyAssetClass :: CurrencySymbol -> TokenName -> String
prittyAssetClass cs TokenName{unTokenName=(BuiltinByteString bs)} = show cs <> "." <> (T.unpack $ E.decodeUtf8 $ Hex.encode bs)

prittyTokenFullValue :: String -> (CurrencySymbol, TokenName, Integer) -> String
prittyTokenFullValue acc (cs, tn, i) = 
  mconcat 
    [ acc
    , "+"
    , "\""
    , show i
    , " "
    , prittyAssetClass cs tn
    , "\""
    ]