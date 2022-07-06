{-# LANGUAGE ExistentialQuantification #-}

module NetworkAPI.Node.Network where

import Cardano.Api
    ( LocalNodeConnectInfo(LocalNodeConnectInfo),
      CardanoMode,
      ConsensusModeParams,
      NetworkId )
import NetworkAPI.Config.NodeConfig (NodeSocketConfig (NodeSocketConfig, nodeSocketPath))
import qualified Control.Concurrent.STM.TQueue as STM
import System.Posix.Semaphore
import RIO
import System.Posix (unionFileModes, ownerReadMode, ownerWriteMode)
import NetworkAPI.Node.Types (OpearationResult)
import qualified NetworkAPI.Node.Types as NT
import System.Logging.Hlog (MakeLogging (MakeLogging, forComponent), Logging (Logging, infoM))

data Network f = Network
  { run                  :: (LocalNodeConnectInfo CardanoMode -> f OpearationResult) -> f OpearationResult
  , runAsync             :: (LocalNodeConnectInfo CardanoMode -> f OpearationResult) -> f ()
  , runAsyncWithCallback :: (LocalNodeConnectInfo CardanoMode -> f OpearationResult) -> (OpearationResult -> f ()) -> f ()
  }

mkNetwork :: forall i f. (MonadIO i, MonadUnliftIO f) => MakeLogging i f -> NT.NetworkId -> NodeSocketConfig -> ConsensusModeParams CardanoMode -> NetworkId -> i (Network f)
mkNetwork MakeLogging{..} netId NodeSocketConfig{..} cModeParams networkId = do
  logging <- forComponent ("Network" ++ show netId)
  let
    connection = LocalNodeConnectInfo cModeParams networkId nodeSocketPath
    semFlags   = OpenSemFlags True True
    semMode    = unionFileModes ownerReadMode ownerWriteMode
  sem   <- liftIO $ semOpen ("nodeNetwork" ++ show netId) semFlags semMode 1
  queue <- liftIO . atomically $ STM.newTQueue
  pure $ Network
    { run = run' logging netId connection sem
    , runAsync = runAsync' logging netId queue connection sem
    , runAsyncWithCallback = runAsyncWithCallback' logging netId queue connection sem
    }

run'
  :: (MonadUnliftIO f)
  => Logging f
  -> NT.NetworkId
  -> LocalNodeConnectInfo CardanoMode
  -> Semaphore
  -> (LocalNodeConnectInfo CardanoMode -> f OpearationResult)
  -> f OpearationResult
run' Logging{..} netId nodeInfo connSemaphore fa =
  infoM ("Going to run sync task for network." ++ show netId) >> 
    bracket 
      (liftIO $ semThreadWait connSemaphore)
      (\_ -> liftIO $ semPost connSemaphore)
      (\_ -> infoM ("Semaphore for newtrok " ++ show netId ++ " is free. Start task execution") >> fa nodeInfo)

runWithCallback'
  :: (MonadIO f, MonadUnliftIO f)
  => Logging f
  -> NT.NetworkId
  -> LocalNodeConnectInfo CardanoMode
  -> Semaphore
  -> (LocalNodeConnectInfo CardanoMode -> f OpearationResult)
  -> (OpearationResult -> f ())
  -> f ()
runWithCallback' Logging{..} netId nodeInfo connSemaphore fa callback =
  infoM ("Going to run sync task with callback for network." ++ show netId) >> 
    bracket 
      (liftIO $ semThreadWait connSemaphore)
      (\_ -> liftIO $ semPost connSemaphore)
      (\_ -> fa nodeInfo >>= callback)

runAsync'
  :: (MonadIO f, MonadUnliftIO f)
  => Logging f
  -> NT.NetworkId
  -> STM.TQueue (LocalNodeConnectInfo CardanoMode -> f OpearationResult, OpearationResult -> f ())
  -> LocalNodeConnectInfo CardanoMode
  -> Semaphore
  -> (LocalNodeConnectInfo CardanoMode -> f OpearationResult)
  -> f ()
runAsync' logging@Logging{..} id tasks nodeInfo connSemaphore fa =
  infoM ("Going to run async for network " ++ show id) >> 
    runAsyncWithCallback' logging id tasks nodeInfo connSemaphore fa (\result -> infoM ("Result of running task: " ++ show result))

runAsyncWithCallback'
  :: (MonadIO f, MonadUnliftIO f)
  => Logging f
  -> NT.NetworkId
  -> STM.TQueue (LocalNodeConnectInfo CardanoMode -> f OpearationResult, OpearationResult -> f ())
  -> LocalNodeConnectInfo CardanoMode
  -> Semaphore
  -> (LocalNodeConnectInfo CardanoMode -> f OpearationResult)
  -> (OpearationResult -> f ())
  -> f ()
runAsyncWithCallback' logging@Logging{..} id tasks nodeInfo connSemaphore fa callback =
  infoM ("Going to run async task with callback for network " ++ show id) >>
    (liftIO . atomically $ writeTQueue tasks (fa, callback)) >> runAsyncWithQueue logging id tasks nodeInfo connSemaphore

runAsyncWithQueue
  :: (MonadIO f, MonadUnliftIO f)
  => Logging f
  -> NT.NetworkId
  -> STM.TQueue (LocalNodeConnectInfo CardanoMode -> f OpearationResult, OpearationResult -> f ())
  -> LocalNodeConnectInfo CardanoMode
  -> Semaphore
  -> f ()
runAsyncWithQueue Logging{..} netId tasks nodeInfo connSemaphore =
  async (
    bracket
      (liftIO $ semThreadWait connSemaphore)
      (\_ -> liftIO $ semPost connSemaphore)
      (\_ -> do
        _ <- infoM ("Semaphore for network " ++ show netId ++ " is free. Going to run task")
        nextTask <- liftIO . atomically $ tryReadTQueue tasks
        case nextTask of
          Nothing   -> infoM ("Tasks queue for " ++ show netId ++ " is empty") >> pure ()
          Just (task, callback) -> 
            infoM ("Run task from queue for " ++ show netId) >> 
              handle 
                (\(e :: SomeException) -> infoM ("Error during runAsyncWithCallback with free semaphore. Error: " ++ show e))
                (task nodeInfo >>= callback)
      )
  ) >> pure ()