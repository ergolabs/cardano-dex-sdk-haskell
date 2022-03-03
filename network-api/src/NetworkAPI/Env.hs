module NetworkAPI.Env where

import qualified Data.Set        as Set
import           Numeric.Natural
import           GHC.Natural

import          Cardano.Api
import          Cardano.Api.Shelley   (ProtocolParameters, PoolId)
import          Cardano.Slotting.Time (SystemStart)

import qualified Explorer.Models as Explorer

data SystemEnv = SystemEnv
  { pparams           :: ProtocolParameters
  , network           :: NetworkId
  , sysstart          :: SystemStart
  , pools             :: Set.Set PoolId
  , eraHistory        :: EraHistory CardanoMode
  , collateralPercent :: Natural
  } deriving (Show)

toNetworkApiSystemEnv :: Explorer.SystemEnv -> SystemEnv
toNetworkApiSystemEnv explorerSysenv =
  SystemEnv 
    { pparams           = Explorer.pparams' explorerSysenv
    , network           = Explorer.network' explorerSysenv
    , sysstart          = Explorer.sysstart' explorerSysenv
    , pools             = Explorer.pools' explorerSysenv
    , eraHistory        = Explorer.eraHistory' explorerSysenv
    , collateralPercent = intToNatural $ Explorer.collateralPercent' explorerSysenv
    }