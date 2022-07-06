module NetworkAPI.PoolsConnector where

import ErgoDex.Amm.Pool (Pool (Pool, poolId), PoolId)
import Data.Sequence
import Data.Map as Map
import RIO ( MonadThrow (throwM), MonadIO, MVar, MonadUnliftIO, Chan, newChan, newMVar, newEmptyMVar, (<&>), writeList2Chan, readMVar, modifyMVar_, readChan, writeChan, async, tryTakeMVar, fromMaybe, isEmptyMVar, putMVar, takeMVar)
import qualified NetworkAPI.Node.Types as NT
import NetworkAPI.Config.NodesSocketsConfig (NodesSocketsConfig (NodesSocketsConfig, nodes))
import Cardano.Api
import NetworkAPI.Node.Network (Network (Network, run, runAsync), mkNetwork)
import System.Logging.Hlog (MakeLogging (MakeLogging, forComponent), Logging (Logging, infoM))

data PoolsConnector f = PoolsConnector
  { runAsyncWithPoolConnection :: Pool -> (LocalNodeConnectInfo CardanoMode -> f NT.OpearationResult) -> f ()
  , runWithPoolConnection      :: Pool -> (LocalNodeConnectInfo CardanoMode -> f NT.OpearationResult) -> f NT.OpearationResult
  , runOnRandomConnection      :: (LocalNodeConnectInfo CardanoMode -> f NT.OpearationResult) -> f NT.OpearationResult
  }

mkPoolsConnector :: forall i f. (MonadIO i, MonadThrow f, MonadUnliftIO f, Enum NT.NetworkId, Num NT.NetworkId)
  => MakeLogging i f
  -> NodesSocketsConfig
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> i (PoolsConnector f)
mkPoolsConnector mkLogging@MakeLogging{..} NodesSocketsConfig{..} cMode networkId = do
  logging <- forComponent "poolsConnector"
  let nodesWithId = Prelude.zip [0..] nodes
  networks <- (\(netId, socketCfg) -> mkNetwork mkLogging netId socketCfg cMode networkId) `traverse` nodesWithId
  let networksWithId = Map.fromList $ Prelude.zip ([0..] <&> NT.NetworkId) networks
  poolsLinks <- newMVar Map.empty
  networkLinks <- newMVar networksWithId
  rotationChan <- newChan
  _ <- writeList2Chan rotationChan (Map.keys networksWithId)
  pure $ PoolsConnector
    { runAsyncWithPoolConnection = runAsyncWithPoolConnection' logging poolsLinks networkLinks rotationChan
    , runWithPoolConnection = runWithPoolConnection' logging poolsLinks networkLinks rotationChan
    , runOnRandomConnection = runOnRandomConnection' networkLinks
    }

runAsyncWithPoolConnection' :: forall f. (MonadThrow f, MonadUnliftIO f)
  => Logging f
  -> MVar (Map.Map PoolId NT.NetworkId)
  -> MVar (Map.Map NT.NetworkId (Network f))
  -> Chan NT.NetworkId -- for networks rotation
  -> Pool
  -> (LocalNodeConnectInfo CardanoMode -> f NT.OpearationResult)
  -> f ()
runAsyncWithPoolConnection' Logging{..} connectionsMap networksMap rotationChan Pool{..} fa =
  infoM ("Going to run action with pool " ++ show poolId) >> async ( do
  _ <- infoM @String "Going to read connections map "
  connections <- readMVar connectionsMap
  _ <- infoM @String ("Current connection map is " ++ show connections)
  networks    <- readMVar networksMap
  let networkM = Map.lookup poolId connections >>= (`Map.lookup` networks)
  case networkM of
    Nothing -> do
      nextNetworkId <- readChan rotationChan
      _             <- infoM @String ("No network for pool " ++ show poolId ++ ". Going to use newtwork under id:" ++ show nextNetworkId)
      _             <- writeChan rotationChan nextNetworkId
      case Map.lookup nextNetworkId networks of
        Nothing -> infoM @String ("No network under id " ++ show nextNetworkId)
        Just Network{..} -> do
          _ <- infoM ("New Newtork for pool " ++ show poolId ++ " is " ++ show nextNetworkId)
          _ <- takeMVar connectionsMap >>= (putMVar connectionsMap . Map.insert poolId nextNetworkId)
          _ <- infoM ("Going to run action with pool " ++ show poolId ++ " on network " ++ show nextNetworkId)
          runAsync fa
    Just Network{..} -> infoM ("Newtork for pool " ++ show poolId ++ " exists") >> runAsync fa) >> pure ()

runWithPoolConnection' :: (MonadThrow f, MonadUnliftIO f)
  => Logging f
  -> MVar (Map.Map PoolId NT.NetworkId)
  -> MVar (Map.Map NT.NetworkId (Network f))
  -> Chan NT.NetworkId -- for networks rotation
  -> Pool
  -> (LocalNodeConnectInfo CardanoMode -> f NT.OpearationResult)
  -> f NT.OpearationResult
runWithPoolConnection' Logging{..} connectionsMap networksMap rotationChan Pool{..} fa = do
  _ <- infoM ("Going to run action with pool " ++ show poolId)
  connections <- readMVar connectionsMap
  _ <- infoM ("Current connection map is " ++ show connections)
  networks    <- readMVar networksMap
  let networkM = Map.lookup poolId connections >>= (`Map.lookup` networks)
  case networkM of
    Nothing -> do
      nextNetworkId <- readChan rotationChan
      _             <- infoM ("No network for pool " ++ show poolId ++ ". Going to use newtwork under id:" ++ show nextNetworkId)
      _             <- writeChan rotationChan nextNetworkId
      case Map.lookup nextNetworkId networks of
        Nothing -> throwM (NT.NetworkNotFound nextNetworkId)
        Just Network{..} -> do
          _ <- infoM ("New Newtork for pool " ++ show poolId ++ " is " ++ show nextNetworkId)
          _ <- takeMVar connectionsMap >>= (putMVar connectionsMap . Map.insert poolId nextNetworkId)
          _ <- infoM ("Going to run action with pool " ++ show poolId ++ " on network " ++ show nextNetworkId)
          run fa
    Just Network{..} -> infoM ("Newtork for pool " ++ show poolId ++ " exists") >> run fa

runOnRandomConnection' :: (MonadThrow f, MonadUnliftIO f)
  => MVar (Map.Map NT.NetworkId (Network f))
  -> (LocalNodeConnectInfo CardanoMode -> f NT.OpearationResult)
  -> f NT.OpearationResult
runOnRandomConnection' networksMap fa = do
  networks <- readMVar networksMap
  let Network{..} = head $ Map.elems networks
  run fa