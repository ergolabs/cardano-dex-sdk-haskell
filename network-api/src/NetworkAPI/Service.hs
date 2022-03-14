module NetworkAPI.Service where

import qualified Cardano.Api    as C
import           NetworkAPI.Config.NodeConfig
import           Cardano.Api
import           RIO
import           Network.HTTP.Simple
import           Data.ByteString.Char8
import           GHC.Natural
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as T

import Explorer.Service as Explorer
import NetworkAPI.Env

data Network f = Network
  { getSystemEnv :: f SystemEnv
  , submitTx     :: C.Tx C.AlonzoEra -> f ()
  }

mkNetwork :: (MonadIO f) => NodeConfig -> Explorer f -> Network f
mkNetwork cfg explorer = Network (Explorer.getSystemEnv explorer <&> toNetworkApiSystemEnv) (submitTx' cfg)

submitTx' :: (MonadIO f) => NodeConfig -> C.Tx C.AlonzoEra -> f ()
submitTx' NodeConfig{..} tx = do
  liftIO . print $ "Going to submit tx to cardano node"
  let
    res = serialiseToCBOR tx
    serialisedTx = Lazy.fromStrict $ serialiseToCBOR tx --check
    encoded = T.decodeUtf8 . Hex.encode $ res
    deser        = deserialiseFromCBOR AsAlonzoTx res
    request = defaultRequest
      & setRequestPath (pack "api/submit/tx")
      & setRequestHost (pack host)
      & setRequestPort (naturalToInt port)
      & setRequestHeader "Content-Type" ["application/cbor"]
      & setRequestMethod (pack "POST")
      & setRequestBodyLBS serialisedTx
  liftIO . print $ ("res: " ++ (show res))
  liftIO . print $ ("serialisedTx: " ++ (show serialisedTx))
  liftIO . print $ ("DeserTx: " ++ (show deser))
  response <- liftIO (httpJSON request :: IO (Response String))
  liftIO $ print $ "Response after tx submit is: " ++ getResponseBody response