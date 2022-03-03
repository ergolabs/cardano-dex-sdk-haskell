module Explorer.Service where

import           Control.Monad.IO.Class
import           Data.ByteString.Char8   as Data
import           Data.Function
import           Data.Aeson
import qualified Data.Text               as T
import           Control.Monad.Catch
import           GHC.Natural
import           Network.HTTP.Simple

import Common.Throw.Combinators

import Explorer.Types
import Explorer.Models
import Explorer.Config

data Explorer f = Explorer
  { getUnspentOutputs        :: Gix -> Limit -> f (Items FullTxOut)
  , getUnspentOutputsByPCred :: PaymentCred -> Paging -> f (Items FullTxOut)
  , getSystemEnv             :: f SystemEnv
  }

mkExplorer :: (MonadIO f, MonadThrow f) => ExplorerConfig -> Explorer f
mkExplorer conf = Explorer
  { getUnspentOutputs        = getUnspentOutputs' conf
  , getUnspentOutputsByPCred = getUnspentOutputsByPCred' conf
  , getSystemEnv             = getSystemEnv' conf
  }

getUnspentOutputs' :: (MonadIO f, MonadThrow f) => ExplorerConfig -> Gix -> Limit -> f (Items FullTxOut)
getUnspentOutputs' conf minIndex limit =
  mkGetRequest conf $ "/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit

getUnspentOutputsByPCred' :: (MonadIO f, MonadThrow f) => ExplorerConfig -> PaymentCred -> Paging -> f (Items FullTxOut)
getUnspentOutputsByPCred' conf pcred Paging{..} =
  mkGetRequest conf $ "/outputs/unspent/byAddr/" ++ (T.unpack $ unPaymentCred pcred) ++  "/?offset=" ++ show offset ++ "&limit=" ++ show limit

getSystemEnv' :: (MonadIO f, MonadThrow f) => ExplorerConfig -> f SystemEnv
getSystemEnv' conf = mkGetRequest conf "/networkParams"

mkGetRequest :: (MonadIO f, FromJSON a, MonadThrow f) => ExplorerConfig -> String -> f a
mkGetRequest ExplorerConfig{..} path = do
  request' <- parseRequestThrow ("GET " <> explorerHost)
  let
    request =
      setRequestPath (Data.pack path)
      $ setRequestPort (naturalToInt explorerPort)
      $ request'

  responseE <- httpJSONEither request
  response  <- mapM throwEither responseE
  pure $ getResponseBody response
