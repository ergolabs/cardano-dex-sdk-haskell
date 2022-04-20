module Explorer.Config where

import GHC.Natural
import GHC.Generics
import Dhall

data ExplorerConfig = ExplorerConfig
  { explorerUrl             :: String
  , maxRetries              :: Natural
  , exponentialBackoffDelay :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerConfig