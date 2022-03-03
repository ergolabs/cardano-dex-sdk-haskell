module WalletAPI.Utxos where

import           RIO
import qualified Data.Set                   as Set
import           Data.ByteArray.Encoding    (Base(..), convertToBase)
import qualified Data.Text.Encoding         as T


import           Ledger
import           Plutus.V1.Ledger.Value
import           Cardano.Api            hiding (Value)

import           CardanoTx.Models
import           WalletAPI.UtxoStore

import           Explorer.Service
import qualified Explorer.Types       as Explorer
import qualified Explorer.Models      as Explorer
import qualified Explorer.Class       as Explorer

data WalletOutputs f = WalletOutputs
  { selectUtxos          :: Value               -> f (Maybe (Set.Set FullTxOut)),
    selectUxtosByFilter  :: (FullTxOut -> Bool) -> f (Maybe (Set.Set FullTxOut))
  }

mkWalletOutputs :: MonadIO f => Explorer f -> UtxoStore f -> Hash PaymentKey -> WalletOutputs f
mkWalletOutputs explorer ustore pkh = WalletOutputs
  { selectUtxos = selectUtxos'' explorer ustore pkh
  , selectUxtosByFilter = selectUxtosByFilter'' explorer ustore pkh
  }

selectUtxos'' :: Monad f => Explorer f -> UtxoStore f -> Hash PaymentKey -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos'' explorer@Explorer{..} ustore@UtxoStore{..} pkh requiredValue = do
  let
    fetchUtxos offset limit = do
      let
        paging  = Explorer.Paging offset limit
        mkPCred = Explorer.PaymentCred . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes
      utxoBatch <- getUnspentOutputsByPCred (mkPCred pkh) paging
      putUtxos (Set.fromList $ Explorer.items utxoBatch <&> Explorer.toCardanoTx)
      let entriesLeft = Explorer.total utxoBatch - (offset + limit)

      if entriesLeft > 0
      then fetchUtxos (offset + limit) limit
      else pure ()

    collect :: [FullTxOut] -> Value -> [FullTxOut] -> Maybe [FullTxOut]
    collect acc valueAcc outs =
      case outs of
        fout@FullTxOut{..} : tl | valueAcc `lt` requiredValue ->
          if Set.null $ Set.intersection (extractAssets fullTxOutValue) (extractAssets requiredValue)
          then collect acc valueAcc tl -- current output doesn't contain the required asset at all, so skipping it
          else collect (fout : acc) (fullTxOutValue <> valueAcc) tl -- need more outputs
          where
            extractAssets v = Set.fromList (flattenValue v <&> (\(cs, tn, _) -> (cs, tn)))
        [] | valueAcc `lt` requiredValue ->
          Nothing
        _ ->
          Just acc

  utxos <- getUtxos
  case collect [] mempty (Set.elems utxos) of
    Just outs -> pure $ Just $ Set.fromList outs
    Nothing   -> fetchUtxos 0 batchSize >> selectUtxos'' explorer ustore pkh requiredValue
      where batchSize = 20

selectUxtosByFilter'' :: forall f. MonadIO f => Explorer f -> UtxoStore f -> Hash PaymentKey -> (FullTxOut -> Bool) -> f (Maybe (Set.Set FullTxOut))
selectUxtosByFilter'' explorer@Explorer{..} ustore@UtxoStore{..} pkh predicate = do
  let
    paging  = Explorer.Paging 0 20
    mkPCred = Explorer.PaymentCred "addr_test1qrt56fk9q2w09yqffl8p5pnsmfeknwgzv4calwthcmazxnl0pwy4t9rk9qlzhd49k40p0yxrsm5c5f8puxlxc9hrqj4sy2tu8f" -- debug . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes
  _   <- liftIO $ print ("paging: " ++ (show paging))
  _   <- liftIO $ print ("mkPCred: " ++ (show (mkPCred)))
  utxoBatch <- getUnspentOutputsByPCred (mkPCred) paging
  let utxos = map (Explorer.toCardanoTx) (Explorer.items utxoBatch)
      filtered = RIO.filter predicate utxos
  if (null filtered)
  then pure Nothing
  else pure $ Just $ Set.fromList filtered
