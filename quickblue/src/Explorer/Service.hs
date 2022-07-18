module Explorer.Service where

import Control.Monad.IO.Class
import Data.ByteString.Char8  as Data
import Data.Function
import Data.Aeson
import qualified  Data.Text as T
import Network.HTTP.Simple

import Explorer.Types
import Explorer.Models
import Explorer.Config

import Ledger ( TxOutRef, txOutRefId, txOutRefIdx )
import Prelude hiding (Ordering)
import System.Logging.Hlog (Logging (Logging, debugM), MakeLogging (MakeLogging, forComponent))

data Explorer f = Explorer
  { getOutput                :: TxOutRef -> f (Maybe FullTxOut)
  , getUnspentOutputs        :: Gix -> Limit -> Ordering -> f (Items FullTxOut)
  , getUnspentOutputsByPCred :: PaymentCred -> Paging -> f (Items FullTxOut)
  , getSystemEnv             :: f SystemEnv
  , getTxs                   :: Paging -> Ordering -> f (Items FullTx)
  }

mkExplorer :: (Monad i, MonadIO f) => MakeLogging i f -> ExplorerConfig -> i (Explorer f)
mkExplorer MakeLogging{..} conf = do
  logging <- forComponent "explorer"
  pure $ Explorer
    { getOutput                = getOutput' logging conf
    , getUnspentOutputs        = getUnspentOutputs' logging conf
    , getUnspentOutputsByPCred = getUnspentOutputsByPCred' logging conf
    , getSystemEnv             = getSystemEnv' logging conf
    , getTxs                   = getTxs' logging conf
    }

getOutput' :: MonadIO f => Logging f -> ExplorerConfig -> TxOutRef -> f (Maybe FullTxOut)
getOutput' logging conf ref =
  mkGetRequest logging conf $ "/v1/outputs/" ++ renderTxOutRef ref

getUnspentOutputs' :: MonadIO f => Logging f -> ExplorerConfig -> Gix -> Limit -> Ordering -> f (Items FullTxOut)
getUnspentOutputs' logging conf minIndex limit ordering =
  mkGetRequest logging conf $ "/v1/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit ++ "&ordering=" ++ show ordering

getUnspentOutputsByPCred' :: MonadIO f => Logging f -> ExplorerConfig -> PaymentCred -> Paging -> f (Items FullTxOut)
getUnspentOutputsByPCred' logging conf pcred Paging{..} =
  mkGetRequest logging conf $ "/v1/outputs/unspent/byPaymentCred/" ++ T.unpack (unPaymentCred pcred) ++  "/?offset=" ++ show offset ++ "&limit=" ++ show limit

getSystemEnv' :: MonadIO f => Logging f -> ExplorerConfig -> f SystemEnv
getSystemEnv' logging conf = mkGetRequest logging conf "/networkParams"

getTxs' :: MonadIO f => Logging f -> ExplorerConfig -> Paging -> Ordering -> f (Items FullTx)
getTxs' logging conf Paging{..} ordering =
  mkGetRequest logging conf $ "/v1/transactions/?offset=" ++ show offset ++ "&limit=" ++ show limit ++ "&ordering=" ++ show ordering

mkGetRequest :: (MonadIO f, FromJSON a, Show a) => Logging f -> ExplorerConfig -> String -> f a
mkGetRequest Logging{..} ExplorerConfig{..} path = do
  let request = parseRequest_ (unUri explorerUri) & setRequestPath (Data.pack path)

  response <- httpJSON request

  let parsedResponse = getResponseBody response

  debugM ("Response is: " ++ show parsedResponse)

  pure parsedResponse

renderTxOutRef :: TxOutRef -> [Char]
renderTxOutRef ref = (show . txOutRefId $ ref) ++ T.unpack txOutRefSep ++ (show . txOutRefIdx $ ref)
