module NetworkAPI.NetworkService
  ( NetworkService(..)
  , mkNetworkService
  ) where

import Cardano.Api
import NetworkAPI.Types (SystemEnv)
import System.Logging.Hlog
import RIO
import NetworkAPI.Node.NodeClient (NodeClient(..))
import Cardano.Api
import Network.Mux
import Control.Monad.Catch (MonadMask)
import Control.Concurrent (forkIO)

data NetworkService f era = NetworkService
  { getSystemEnv :: f SystemEnv
  , submitTx     :: Tx era -> f ()
  }

mkNetworkService :: (MonadIO i, MonadMask f, MonadUnliftIO f) => MakeLogging i f -> NodeClient f era -> i (NetworkService f era)
mkNetworkService MakeLogging{..} client = do
  logging   <- forComponent "networkService"
  emptyMVar <- newEmptyMVar
  pure $ NetworkService
    { getSystemEnv = getSystemEnv' logging client emptyMVar
    , submitTx     = submitTx' logging client
    }

getSystemEnv'
   :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
   => Logging f
   -> NodeClient f era
   -> MVar SystemEnv
   -> f SystemEnv
getSystemEnv' logging client mVar = do
  mVarReadResult <- liftIO $ tryTakeMVar mVar
  case mVarReadResult of
    Nothing -> do
      env <- getSystemEnvFromNode logging client
      _   <- liftIO $ tryPutMVar mVar env
      return env
    Just env -> async (updateSystemEnv' logging client mVar) >> pure env

updateSystemEnv'
  :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
  => Logging f
  -> NodeClient f era
  -> MVar SystemEnv
  -> f ()
updateSystemEnv' logging@Logging{..} client mVar = do
  _  <- infoM @String "Going to update system env"
  newSystemEnv <- getSystemEnvFromNode logging client
  putResult    <- liftIO $ tryPutMVar mVar newSystemEnv
  infoM ("Result of updating systemEnv is " ++ show putResult)

getSystemEnvFromNode
   :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
   => Logging f
   -> NodeClient f era
   -> f SystemEnv
getSystemEnvFromNode logging@Logging{..} client@NodeClient{..} = do
  catch getSystemEnv (\err ->
    case err of
      (MuxError MuxBearerClosed _) -> do
        debugM @String "Mux burer error during getSetemEnv receiving. Trying to get new one"
        getSystemEnvFromNode logging client -- try to get new data
      _ -> do
        debugM ("Error in getSystemEnv err:" ++ show err)
        throwM err
   )

submitTx'
  :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
  => Logging f
  -> NodeClient f era
  -> Tx era
  -> f ()
submitTx' Logging{..} NodeClient{..} tx =
  catch (submitTx tx) (\err ->
    case err of
      (MuxError MuxBearerClosed _) -> infoM @String "Mux burer error in submit tx. Ignore it" -- ignore this kind of errs
      aE -> do
        errorM ("Not MuxError MuxBearerClosed during tx submiting:" ++ show aE)
        throwM err
  )