{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Network where

import           Data.Aeson (decode)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

import NetworkAPI.Service
import NetworkAPI.Types

import qualified Cardano.Api         as C
import           Cardano.Api         (CardanoMode, NetworkId(..), EraHistory(..), CardanoMode)
import           Cardano.Api.Shelley (
  ProtocolParameters(..),
  ExecutionUnitPrices(..),
  ExecutionUnits(..),
  PoolId,
  AnyPlutusScriptVersion(..),
  PlutusScriptV1(..),
  CostModel(..))

import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Summary
import           Ouroboros.Consensus.HardFork.History.EraParams
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Counting

mkMockNetwork :: Applicative f => CardanoNetwork f era
mkMockNetwork = CardanoNetwork
  { getSystemEnv = pure staticSystemEnv
  , submitTx     = const $ \_ -> pure ()
  }

staticSystemEnv :: SystemEnv
staticSystemEnv = SystemEnv
  { pparams    = staticProtocolParams
  , sysstart   = undefined
  , pools      = mempty
  , eraHistory = staticEraHistory
  }

staticEraHistory :: EraHistory CardanoMode
staticEraHistory =
  EraHistory
    C.CardanoMode
    (History.mkInterpreter $ Summary $ NonEmptyOne $
      EraSummary
        { eraStart  = initBound
        , eraEnd    = EraUnbounded
        , eraParams = EraParams
          { eraEpochSize  = EpochSize 100
          , eraSlotLength = mkSlotLength 100
          , eraSafeZone   = UnsafeIndefiniteSafeZone
          }
        }
    )

costModel :: Map.Map AnyPlutusScriptVersion CostModel
costModel = fromMaybe mempty (decode @(Map.Map AnyPlutusScriptVersion CostModel) costModelJson)

staticProtocolParams :: ProtocolParameters
staticProtocolParams = ProtocolParameters
  { protocolParamProtocolVersion = (6, 0)
  , protocolParamDecentralization = 0
  , protocolParamExtraPraosEntropy = Nothing
  , protocolParamMaxBlockHeaderSize = 1100
  , protocolParamMaxBlockBodySize = 98304
  , protocolParamMaxTxSize = 16384
  , protocolParamTxFeeFixed = 44
  , protocolParamTxFeePerByte = 155381
  , protocolParamMinUTxOValue = Just 34482
  , protocolParamStakeAddressDeposit = 2000000
  , protocolParamStakePoolDeposit = 500000000
  , protocolParamMinPoolCost = 340000000
  , protocolParamPoolRetireMaxEpoch = 18
  , protocolParamStakePoolTargetNum = 500
  , protocolParamPoolPledgeInfluence = 0.3
  , protocolParamMonetaryExpansion = 0.003
  , protocolParamTreasuryCut = 0.2
  , protocolParamUTxOCostPerWord = Just 34482
  , protocolParamCostModels = costModel
  , protocolParamPrices = Just $ ExecutionUnitPrices 1 1
  , protocolParamMaxTxExUnits = Just $ ExecutionUnits 10000000000 16000000
  , protocolParamMaxBlockExUnits = Just $ ExecutionUnits 40000000000 80000000
  , protocolParamMaxValueSize = Just 1000
  , protocolParamCollateralPercent = Just 100
  , protocolParamMaxCollateralInputs = Just 1
  }

costModelJson =
  "{\"PlutusV1\": {\"bData-cpu-arguments\": 150000, \"iData-cpu-arguments\": 150000, \"trace-cpu-arguments\": 150000, \"mkCons-cpu-arguments\": 150000, \"fstPair-cpu-arguments\": 150000, \"mapData-cpu-arguments\": 150000, \"sndPair-cpu-arguments\": 150000, \"unBData-cpu-arguments\": 150000, \"unIData-cpu-arguments\": 150000, \"bData-memory-arguments\": 32, \"cekLamCost-exBudgetCPU\": 29773, \"cekVarCost-exBudgetCPU\": 29773, \"headList-cpu-arguments\": 150000, \"iData-memory-arguments\": 32, \"listData-cpu-arguments\": 150000, \"nullList-cpu-arguments\": 150000, \"tailList-cpu-arguments\": 150000, \"trace-memory-arguments\": 32, \"mkCons-memory-arguments\": 32, \"mkNilData-cpu-arguments\": 150000, \"unMapData-cpu-arguments\": 150000, \"blake2b-memory-arguments\": 4, \"cekApplyCost-exBudgetCPU\": 29773, \"cekConstCost-exBudgetCPU\": 29773, \"cekDelayCost-exBudgetCPU\": 29773, \"cekForceCost-exBudgetCPU\": 29773, \"chooseData-cpu-arguments\": 150000, \"chooseList-cpu-arguments\": 150000, \"chooseUnit-cpu-arguments\": 150000, \"constrData-cpu-arguments\": 150000, \"fstPair-memory-arguments\": 32, \"ifThenElse-cpu-arguments\": 1, \"mapData-memory-arguments\": 32, \"mkPairData-cpu-arguments\": 150000, \"sndPair-memory-arguments\": 32, \"unBData-memory-arguments\": 32, \"unIData-memory-arguments\": 32, \"unListData-cpu-arguments\": 150000, \"cekLamCost-exBudgetMemory\": 100, \"cekVarCost-exBudgetMemory\": 100, \"headList-memory-arguments\": 32, \"listData-memory-arguments\": 32, \"nullList-memory-arguments\": 32, \"sha2_256-memory-arguments\": 4, \"sha3_256-memory-arguments\": 4, \"tailList-memory-arguments\": 32, \"cekBuiltinCost-exBudgetCPU\": 29773, \"cekStartupCost-exBudgetCPU\": 100, \"mkNilData-memory-arguments\": 32, \"unConstrData-cpu-arguments\": 150000, \"unMapData-memory-arguments\": 32, \"blake2b-cpu-arguments-slope\": 29175, \"cekApplyCost-exBudgetMemory\": 100, \"cekConstCost-exBudgetMemory\": 100, \"cekDelayCost-exBudgetMemory\": 100, \"cekForceCost-exBudgetMemory\": 100, \"chooseData-memory-arguments\": 32, \"chooseList-memory-arguments\": 32, \"chooseUnit-memory-arguments\": 32, \"constrData-memory-arguments\": 32, \"equalsData-memory-arguments\": 1, \"ifThenElse-memory-arguments\": 1, \"mkNilPairData-cpu-arguments\": 150000, \"mkPairData-memory-arguments\": 32, \"unListData-memory-arguments\": 32, \"sha2_256-cpu-arguments-slope\": 29175, \"sha3_256-cpu-arguments-slope\": 82363, \"cekBuiltinCost-exBudgetMemory\": 100, \"cekStartupCost-exBudgetMemory\": 100, \"equalsString-memory-arguments\": 1, \"indexByteString-cpu-arguments\": 150000, \"unConstrData-memory-arguments\": 32, \"addInteger-cpu-arguments-slope\": 0, \"decodeUtf8-cpu-arguments-slope\": 1000, \"encodeUtf8-cpu-arguments-slope\": 1000, \"equalsData-cpu-arguments-slope\": 10000, \"equalsInteger-memory-arguments\": 1, \"mkNilPairData-memory-arguments\": 32, \"blake2b-cpu-arguments-intercept\": 2477736, \"appendString-cpu-arguments-slope\": 1000, \"equalsString-cpu-arguments-slope\": 1000, \"indexByteString-memory-arguments\": 1, \"lengthOfByteString-cpu-arguments\": 150000, \"lessThanInteger-memory-arguments\": 1, \"sha2_256-cpu-arguments-intercept\": 2477736, \"sha3_256-cpu-arguments-intercept\": 0, \"verifySignature-memory-arguments\": 1, \"addInteger-memory-arguments-slope\": 1, \"decodeUtf8-memory-arguments-slope\": 8, \"encodeUtf8-memory-arguments-slope\": 8, \"equalsByteString-memory-arguments\": 1, \"equalsInteger-cpu-arguments-slope\": 1326, \"modInteger-cpu-arguments-constant\": 148000, \"modInteger-memory-arguments-slope\": 1, \"addInteger-cpu-arguments-intercept\": 197209, \"consByteString-cpu-arguments-slope\": 1000, \"decodeUtf8-cpu-arguments-intercept\": 150000, \"encodeUtf8-cpu-arguments-intercept\": 150000, \"equalsData-cpu-arguments-intercept\": 150000, \"appendString-memory-arguments-slope\": 1, \"equalsString-cpu-arguments-constant\": 1000, \"lengthOfByteString-memory-arguments\": 4, \"lessThanByteString-memory-arguments\": 1, \"lessThanInteger-cpu-arguments-slope\": 497, \"modInteger-memory-arguments-minimum\": 1, \"multiplyInteger-cpu-arguments-slope\": 11218, \"sliceByteString-cpu-arguments-slope\": 5000, \"subtractInteger-cpu-arguments-slope\": 0, \"verifySignature-cpu-arguments-slope\": 1, \"appendByteString-cpu-arguments-slope\": 621, \"appendString-cpu-arguments-intercept\": 150000, \"divideInteger-cpu-arguments-constant\": 148000, \"divideInteger-memory-arguments-slope\": 1, \"equalsByteString-cpu-arguments-slope\": 247, \"equalsString-cpu-arguments-intercept\": 150000, \"addInteger-memory-arguments-intercept\": 1, \"consByteString-memory-arguments-slope\": 1, \"decodeUtf8-memory-arguments-intercept\": 0, \"encodeUtf8-memory-arguments-intercept\": 0, \"equalsInteger-cpu-arguments-intercept\": 136542, \"modInteger-memory-arguments-intercept\": 0, \"consByteString-cpu-arguments-intercept\": 150000, \"divideInteger-memory-arguments-minimum\": 1, \"lessThanByteString-cpu-arguments-slope\": 248, \"lessThanEqualsInteger-memory-arguments\": 1, \"multiplyInteger-memory-arguments-slope\": 1, \"quotientInteger-cpu-arguments-constant\": 148000, \"quotientInteger-memory-arguments-slope\": 1, \"sliceByteString-memory-arguments-slope\": 1, \"subtractInteger-memory-arguments-slope\": 1, \"appendByteString-memory-arguments-slope\": 1, \"appendString-memory-arguments-intercept\": 0, \"equalsByteString-cpu-arguments-constant\": 150000, \"lessThanInteger-cpu-arguments-intercept\": 179690, \"multiplyInteger-cpu-arguments-intercept\": 61516, \"remainderInteger-cpu-arguments-constant\": 148000, \"remainderInteger-memory-arguments-slope\": 1, \"sliceByteString-cpu-arguments-intercept\": 150000, \"subtractInteger-cpu-arguments-intercept\": 197209, \"verifySignature-cpu-arguments-intercept\": 3345831, \"appendByteString-cpu-arguments-intercept\": 396231, \"divideInteger-memory-arguments-intercept\": 0, \"equalsByteString-cpu-arguments-intercept\": 112536, \"quotientInteger-memory-arguments-minimum\": 1, \"consByteString-memory-arguments-intercept\": 0, \"lessThanEqualsByteString-memory-arguments\": 1, \"lessThanEqualsInteger-cpu-arguments-slope\": 1366, \"remainderInteger-memory-arguments-minimum\": 1, \"lessThanByteString-cpu-arguments-intercept\": 103599, \"multiplyInteger-memory-arguments-intercept\": 0, \"quotientInteger-memory-arguments-intercept\": 0, \"sliceByteString-memory-arguments-intercept\": 0, \"subtractInteger-memory-arguments-intercept\": 1, \"appendByteString-memory-arguments-intercept\": 0, \"remainderInteger-memory-arguments-intercept\": 0, \"lessThanEqualsByteString-cpu-arguments-slope\": 248, \"lessThanEqualsInteger-cpu-arguments-intercept\": 145276, \"modInteger-cpu-arguments-model-arguments-slope\": 118, \"lessThanEqualsByteString-cpu-arguments-intercept\": 103599, \"divideInteger-cpu-arguments-model-arguments-slope\": 118, \"modInteger-cpu-arguments-model-arguments-intercept\": 425507, \"quotientInteger-cpu-arguments-model-arguments-slope\": 118, \"remainderInteger-cpu-arguments-model-arguments-slope\": 118, \"divideInteger-cpu-arguments-model-arguments-intercept\": 425507, \"quotientInteger-cpu-arguments-model-arguments-intercept\": 425507, \"remainderInteger-cpu-arguments-model-arguments-intercept\": 425507}}"
