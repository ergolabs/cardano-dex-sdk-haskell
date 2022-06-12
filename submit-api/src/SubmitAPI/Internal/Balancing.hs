module SubmitAPI.Internal.Balancing where

import Prelude

import           Data.Bifunctor (first)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)
import qualified Debug.Trace    as D
import           Data.Set       (Set)

import Cardano.Api
import Cardano.Api.Shelley   (ProtocolParameters(..), PoolId)
import Cardano.Slotting.Time (SystemStart)

makeTransactionBodyAutoBalance
  :: forall era mode.
     IsShelleyBasedEra era
  => EraInMode era mode
  -> SystemStart
  -> EraHistory mode
  -> ProtocolParameters
  -> Set PoolId       -- ^ The set of registered stake pools
  -> UTxO era         -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> TxBodyContent BuildTx era
  -> AddressInEra era -- ^ Change address
  -> Maybe Word       -- ^ Override key witnesses
  -> Either TxBodyErrorAutoBalance (BalancedTxBody era)
makeTransactionBodyAutoBalance eraInMode systemstart history pparams
                            poolids utxo txbodycontent changeaddr mnkeys = do

    -- Our strategy is to:
    -- 1. evaluate all the scripts to get the exec units, update with ex units
    -- 2. figure out the overall min fees
    -- 3. update tx with fees
    -- 4. balance the transaction and update tx change output

    txbody0 <-
      first TxBodyError $ makeTransactionBody txbodycontent
        { txOuts =
            txOuts txbodycontent ++ [TxOut changeaddr (lovelaceToTxOutValue 0) TxOutDatumNone]
            --TODO: think about the size of the change output
            -- 1,2,4 or 8 bytes?
        }
    exUnitsMap <- first TxBodyErrorValidityInterval $
                    evaluateTransactionExecutionUnits
                      eraInMode
                      systemstart
                      history
                      pparams
                      utxo
                      txbody0
    exUnitsMap'' <-
      case Map.mapEither id exUnitsMap of
        (failures, exUnitsMap') ->
          handleExUnitsErrors
            (txScriptValidityToScriptValidity (txScriptValidity txbodycontent))
            failures
            exUnitsMap'
    let exUnitsMap' = Map.map (\(ExecutionUnits executionSteps executionMemory) -> ExecutionUnits (executionSteps + 4096 + 864) executionMemory) exUnitsMap''
    let txbodycontent1 = substituteExecutionUnits exUnitsMap' txbodycontent
    explicitTxFees <- first (const TxBodyErrorByronEraNotSupported) $
                        txFeesExplicitInEra era'
    -- Make a txbody that we will use for calculating the fees. For the purpose
    -- of fees we just need to make a txbody of the right size in bytes. We do
    -- not need the right values for the fee or change output. We use
    -- "big enough" values for the change output and set so that the CBOR
    -- encoding size of the tx will be big enough to cover the size of the final
    -- output and fee. Yes this means this current code will only work for
    -- final fee of less than around 4000 ada (2^32-1 lovelace) and change output
    -- of less than around 18 trillion ada  (2^64-1 lovelace).
    txbody1 <- first TxBodyError $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee  = TxFeeExplicit explicitTxFees $ Lovelace (2^(32 :: Integer) - 1),
                 txOuts = txOuts txbodycontent ++
                            [TxOut changeaddr
                              (lovelaceToTxOutValue $ Lovelace (2^(64 :: Integer)) - 1)
                              TxOutDatumNone
                            ]
               }
    let nkeys = fromMaybe (estimateTransactionKeyWitnessCount txbodycontent1)
                          mnkeys
        fee   = evaluateTransactionFee pparams txbody1 nkeys 0 --TODO: byron keys
    -- Make a txbody for calculating the balance. For this the size of the tx
    -- does not matter, instead it's just the values of the fee and outputs.
    -- Here we do not want to start with any change output, since that's what
    -- we need to calculate.
    txbody2 <- first TxBodyError $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee = TxFeeExplicit explicitTxFees fee
               }

    _ <- D.traceM ("txbody2: " ++ (show txbody2))
    let balance = evaluateTransactionBalance pparams poolids utxo txbody2
    _ <- D.traceM ("Balance: " ++ (show balance))
    mapM_ (`checkMinUTxOValue` pparams) $ txOuts txbodycontent1

    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    case balance of
      TxOutAdaOnly _ _ -> balanceCheck balance
      TxOutValue _ v   ->
        case valueToLovelace v of
          Nothing -> Left $ TxBodyErrorNonAdaAssetsUnbalanced v
          Just _  -> balanceCheck balance

    --TODO: we could add the extra fee for the CBOR encoding of the change,
    -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.

    -- The txbody with the final fee and change output. This should work
    -- provided that the fee and change are less than 2^32-1, and so will
    -- fit within the encoding size we picked above when calculating the fee.
    -- Yes this could be an over-estimate by a few bytes if the fee or change
    -- would fit within 2^16-1. That's a possible optimisation.
    txbody3 <-
      first TxBodyError $ -- TODO: impossible to fail now
        makeTransactionBody txbodycontent1 {
          txFee  = TxFeeExplicit explicitTxFees fee,
          txOuts = accountForNoChange
                     (TxOut changeaddr balance TxOutDatumNone)
                     (txOuts txbodycontent)
        }
    return (BalancedTxBody txbody3 (TxOut changeaddr balance TxOutDatumNone) fee)
 where
   era :: ShelleyBasedEra era
   era = shelleyBasedEra

   era' :: CardanoEra era
   era' = cardanoEra

   -- In the event of spending the exact amount of lovelace in
   -- the specified input(s), this function excludes the change
   -- output. Note that this does not save any fees because by default
   -- the fee calculation includes a change address for simplicity and
   -- we make no attempt to recalculate the tx fee without a change address.
   accountForNoChange :: TxOut CtxTx era -> [TxOut CtxTx era] -> [TxOut CtxTx era]
   accountForNoChange change@(TxOut _ balance _) rest =
     case txOutValueToLovelace balance of
       Lovelace 0 -> rest
       _          -> rest ++ [change]

   balanceCheck :: TxOutValue era -> Either TxBodyErrorAutoBalance ()
   balanceCheck balance
    | txOutValueToLovelace balance == 0 = return ()
    | txOutValueToLovelace balance < 0 =
        Left . TxBodyErrorAdaBalanceNegative $ txOutValueToLovelace balance
    | otherwise =
        case checkMinUTxOValue (TxOut changeaddr balance TxOutDatumNone) pparams of
          Left (TxBodyErrorMinUTxONotMet txOutAny minUTxO) ->
            Left $ TxBodyErrorAdaBalanceTooSmall txOutAny minUTxO (txOutValueToLovelace balance)
          Left err -> Left err
          Right _  -> Right ()

   checkMinUTxOValue
     :: TxOut CtxTx era
     -> ProtocolParameters
     -> Either TxBodyErrorAutoBalance ()
   checkMinUTxOValue txout@(TxOut _ v _) pparams' = do
     minUTxO  <- first TxBodyErrorMinUTxOMissingPParams
                   $ calculateMinimumUTxO era txout pparams'
     _ <- D.traceM ("minUTxO:" ++ (show minUTxO))
     _ <- D.traceM ("txout in check min value:" ++ (show txout))
     _ <- D.traceM ("txOutValueToLovelace v:" ++ (show (txOutValueToLovelace v)))
     _ <- D.traceM ("selectLovelace minUTxO:" ++ (show (selectLovelace minUTxO)))
     if txOutValueToLovelace v >= selectLovelace minUTxO
     then Right ()
     else D.traceM ("Min value not preserved in utxo:" ++ (show txout)) >> Left TxBodyErrorByronEraNotSupported

substituteExecutionUnits :: Map ScriptWitnessIndex ExecutionUnits
                         -> TxBodyContent BuildTx era
                         -> TxBodyContent BuildTx era
substituteExecutionUnits exUnitsMap =
    mapTxScriptWitnesses f
  where
    f :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> ScriptWitness witctx era
    f _   wit@SimpleScriptWitness{} = wit
    f idx wit@(PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing      -> wit
        Just exunits -> PlutusScriptWitness langInEra version script
                                            datum redeemer exunits

lovelaceToTxOutValue :: IsCardanoEra era => Lovelace -> TxOutValue era
lovelaceToTxOutValue l =
    case multiAssetSupportedInEra cardanoEra of
      Left adaOnly     -> TxOutAdaOnly adaOnly  l
      Right multiAsset -> TxOutValue multiAsset (lovelaceToValue l)

handleExUnitsErrors ::
     ScriptValidity -- ^ Mark script as expected to pass or fail validation
  -> Map ScriptWitnessIndex ScriptExecutionError
  -> Map ScriptWitnessIndex ExecutionUnits
  -> Either TxBodyErrorAutoBalance (Map ScriptWitnessIndex ExecutionUnits)
handleExUnitsErrors ScriptValid failuresMap exUnitsMap =
    if null failures
      then Right exUnitsMap
      else Left (TxBodyScriptExecutionError failures)
  where failures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        failures = Map.toList failuresMap
handleExUnitsErrors ScriptInvalid failuresMap exUnitsMap
  | null scriptFailures = Left TxBodyScriptBadScriptValidity
  | null nonScriptFailures = Right exUnitsMap
  | otherwise = Left (TxBodyScriptExecutionError nonScriptFailures)
  where nonScriptFailures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        nonScriptFailures = filter (not . isScriptErrorEvaluationFailed) (Map.toList failuresMap)
        scriptFailures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        scriptFailures = filter isScriptErrorEvaluationFailed (Map.toList failuresMap)
        isScriptErrorEvaluationFailed :: (ScriptWitnessIndex, ScriptExecutionError) -> Bool
        isScriptErrorEvaluationFailed (_, e) = case e of
            ScriptErrorEvaluationFailed _ _ -> True
            _                               -> True

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone                = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity
