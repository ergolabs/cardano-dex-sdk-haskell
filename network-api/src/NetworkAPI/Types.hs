module NetworkAPI.Types
  ( SocketPath(..)
  , SystemEnv(..)
  ) where

import Data.Set (Set)

import Cardano.Api           (CardanoMode, EraHistory)
import Cardano.Api.Shelley   (ProtocolParameters, PoolId)
import Cardano.Slotting.Time (SystemStart)

newtype SocketPath = SocketPath FilePath
  deriving newtype (Eq, Show)

data SystemEnv = SystemEnv
  { pparams    :: ProtocolParameters
  , sysstart   :: SystemStart
  , pools      :: Set PoolId
  , eraHistory :: EraHistory CardanoMode
  }

instance Show SystemEnv where
  show _ = "SystemEnv" -- todo: only for debug
