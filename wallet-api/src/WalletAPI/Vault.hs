module WalletAPI.Vault where

import           RIO
import qualified Data.Set                   as Set
import qualified Data.List                  as List
import           Data.ByteArray.Encoding    (Base(..), convertToBase)
import qualified Data.Text.Encoding         as T


import           Ledger
import           Plutus.V1.Ledger.Value
import           Cardano.Api            hiding (Value)
import qualified Cardano.Api            as Crypto
import           CardanoTx.Value

import           CardanoTx.Models
import           WalletAPI.TrustStore
import           WalletAPI.UtxoStore
import qualified WalletAPI.Utxos     as U
import           Explorer.Service
import qualified Explorer.Types       as Explorer
import qualified Explorer.Models      as Explorer
import qualified Explorer.Class       as Explorer

data Vault f = Vault
  { getSigningKey        :: PubKeyHash          -> f (Maybe ShelleyWitnessSigningKey)
  , selectUtxos          :: Value               -> f (Maybe (Set.Set FullTxOut))
  , selectUxtosByFilter  :: (FullTxOut -> Bool) -> f (Maybe (Set.Set FullTxOut))
  }

narrowVault :: Vault f -> U.WalletOutputs f
narrowVault Vault{..}= U.WalletOutputs selectUtxos selectUxtosByFilter

mkVault :: MonadIO f => Explorer f -> TrustStore f -> KeyPass -> f (Vault f)
mkVault explorer tstore pass = do
  ustore <- mkUtxoStore
  pure $ Vault
    { getSigningKey       = getSigningKey' tstore pass
    , selectUtxos         = selectUtxos' explorer ustore tstore
    , selectUxtosByFilter = selectUxtosByFilter' explorer ustore tstore
    }

getSigningKey' :: Functor f => TrustStore f -> KeyPass -> PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
getSigningKey' TrustStore{readSK} pass _ = readSK pass <&> WitnessPaymentKey <&> Just

selectUtxos' :: forall f. MonadIO f => Explorer f -> UtxoStore f -> TrustStore f -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos' explorer@Explorer{..} ustore@UtxoStore{..} tstore@TrustStore{..} requiredValue = do
  let
    fetchUtxos offset limit = do
      -- pkh <- readVK <&> Crypto.verificationKeyHash
      let
        paging  = Explorer.Paging offset limit
        mkPCred = Explorer.PaymentCred "addr_test1qrt56fk9q2w09yqffl8p5pnsmfeknwgzv4calwthcmazxnl0pwy4t9rk9qlzhd49k40p0yxrsm5c5f8puxlxc9hrqj4sy2tu8f" -- debug . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes
      _   <- liftIO $ print ("paging: " ++ (show paging))
      _   <- liftIO $ print ("mkPCred: " ++ (show (mkPCred)))
      utxoBatch <- getUnspentOutputsByPCred (mkPCred) paging
      _   <- liftIO $ print ("utxoBatch: " ++ (show utxoBatch))
      putUtxos (Set.fromList $ Explorer.items utxoBatch <&> Explorer.toCardanoTx)
      let entriesLeft = (Explorer.total utxoBatch) - (offset + limit)

      if entriesLeft > 0
      then fetchUtxos (offset + limit) limit
      else pure ()

    collect :: [FullTxOut] -> Value -> [FullTxOut] -> f (Maybe [FullTxOut])
    collect acc valueAcc outs = do
      _ <- liftIO $ print ("acc:" ++ (show acc))
      _ <- liftIO $ print ("valueAcc:" ++ (show valueAcc))
      _ <- liftIO $ print ("outs:" ++ (show outs))
      _ <- liftIO $ print ("requiredValue:" ++ (show requiredValue))
      case outs of
        fout@FullTxOut{..} : tl | valueAcc `lt` requiredValue ->
          if Set.null $ Set.intersection (extractAssets fullTxOutValue) (extractAssets requiredValue)
          then collect acc valueAcc tl -- current output doesn't contain the required asset at all, so skipping it
          else collect (fout : acc) (unionVal fullTxOutValue valueAcc) tl -- need more outputs
          where
            extractAssets v = Set.fromList (flattenValue v <&> (\(cs, tn, _) -> (cs, tn)))
        [] | valueAcc `lt` requiredValue ->
          pure $ Nothing
        _ ->
          pure $ Just acc

  utxos <- getUtxos
  res <- collect [] mempty (Set.elems utxos)
  case res of
    Just outs -> pure $ Just $ Set.fromList outs
    Nothing   -> fetchUtxos 0 batchSize >> selectUtxos' explorer ustore tstore requiredValue
      where batchSize = 20

selectUxtosByFilter' :: forall f. MonadIO f => Explorer f -> UtxoStore f -> TrustStore f -> (FullTxOut -> Bool) -> f (Maybe (Set.Set FullTxOut))
selectUxtosByFilter' explorer@Explorer{..} ustore@UtxoStore{..} tstore@TrustStore{..} predicate = do
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

