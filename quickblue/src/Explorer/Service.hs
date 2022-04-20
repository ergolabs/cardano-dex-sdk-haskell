module Explorer.Service where

import Control.Monad.IO.Class
import Data.ByteString.Char8  as Data
import Data.Function
import Data.Aeson
import qualified  Data.Text as T
import Data.Either.Combinators
import GHC.Natural
import Network.HTTP.Simple
import Control.Monad.Catch
import GHC.Natural
import Control.Retry

import Explorer.Types
import Explorer.Models
import Explorer.Config

import Ledger ( TxOutRef, txOutRefId, txOutRefIdx )

data Explorer f = Explorer
  { getOutput                :: TxOutRef -> f (Maybe FullTxOut)
  , getUnspentOutputs        :: Gix -> Limit -> f (Items FullTxOut)
  , getUnspentOutputsByPCred :: PaymentCred -> Paging -> f (Items FullTxOut)
  , getSystemEnv             :: f SystemEnv
  }

mkExplorer :: (MonadIO f, MonadMask f) => ExplorerConfig -> Explorer f
mkExplorer conf = Explorer
  { getOutput                = getOutput' conf
  , getUnspentOutputs        = getUnspentOutputs' conf
  , getUnspentOutputsByPCred = getUnspentOutputsByPCred' conf
  , getSystemEnv             = getSystemEnv' conf
  }

getOutput' :: (MonadIO f, MonadMask f) => ExplorerConfig -> TxOutRef -> f (Maybe FullTxOut)
getOutput' conf ref =
  mkGetWithRetry conf $ "/v1/outputs/" ++ renderTxOutRef ref

getUnspentOutputs' :: (MonadIO f, MonadMask f) => ExplorerConfig -> Gix -> Limit -> f (Items FullTxOut)
getUnspentOutputs' conf minIndex limit =
  mkGetWithRetry conf $ "/v1/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit

getUnspentOutputsByPCred' :: (MonadIO f, MonadMask f) => ExplorerConfig -> PaymentCred -> Paging -> f (Items FullTxOut)
getUnspentOutputsByPCred' conf pcred Paging{..} =
  mkGetWithRetry conf $ "/v1/outputs/unspent/byPaymentCred/" ++ T.unpack (unPaymentCred pcred) ++  "/?offset=" ++ show offset ++ "&limit=" ++ show limit

getSystemEnv' :: (MonadIO f, MonadMask f) => ExplorerConfig -> f SystemEnv
getSystemEnv' conf = mkGetWithRetry conf "/networkParams"

mkGetWithRetry :: (MonadIO f, FromJSON a, MonadMask f) => ExplorerConfig -> String -> f a
mkGetWithRetry cfg@ExplorerConfig{..} path = do
  recoverAll (exponentialBackoff (naturalToInt exponentialBackoffDelay) <> limitRetries (naturalToInt maxRetries)) toRet
   where
     toRet status = do
       (liftIO . print $ ("Going to get data from explorer. Current retry status: " ++ (show status)))
       mkGetRequest cfg path

mkGetRequest :: (MonadIO f, FromJSON a, MonadMask f) => ExplorerConfig -> String -> f a
mkGetRequest ExplorerConfig{..} path = do
  initReq <- parseRequest explorerUrl
  let
    request = initReq
      & setRequestPath (Data.pack path)
      & setRequestMethod "GET"

  response <- httpJSON request
  pure $ getResponseBody response

renderTxOutRef ref = (show . txOutRefId $ ref) ++ "#" ++ (show . txOutRefIdx $ ref)
