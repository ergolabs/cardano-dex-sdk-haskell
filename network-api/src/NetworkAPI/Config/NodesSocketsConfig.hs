module NetworkAPI.Config.NodesSocketsConfig where

import NetworkAPI.Config.NodeConfig (NodeSocketConfig)
import GHC.Generics (Generic)
import qualified Dhall as D

data NodesSocketsConfig = NodesSocketsConfig
  { nodes :: [NodeSocketConfig]
  } deriving Generic

instance D.FromDhall NodesSocketsConfig