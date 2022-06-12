module NetworkAPI.Config.NodeConfig where

import           GHC.Natural
import           GHC.Generics
import qualified Dhall         as D

data NodeConfig = NodeConfig
  { port :: Natural
  , host :: String
  } deriving Generic

instance D.FromDhall NodeConfig

data NodeSocketConfig = NodeSocketConfig
  { nodeSocketPath :: FilePath
  } deriving (Generic, Show)

instance D.FromDhall NodeSocketConfig