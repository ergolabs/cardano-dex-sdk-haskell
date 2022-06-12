{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NetworkAPI.Node.Types where

import Cardano.Api hiding (NetworkId)
import qualified Cardano.Api as C
import qualified RIO.Prelude as Int
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM))
import NetworkAPI.Types (SystemEnv)

newtype NetworkId = NetworkId Int deriving (Eq,Show,Ord)

data NetworkNotFound = NetworkNotFound NetworkId deriving (Show, Exception)

instance Num NetworkId where
  (+) = \ (NetworkId a) (NetworkId b) -> NetworkId (a + b)
  (*) = \ (NetworkId a) (NetworkId b) -> NetworkId (a * b)
  abs =  \ (NetworkId a) -> NetworkId (abs a)
  signum = \ (NetworkId a) -> NetworkId (signum a)
  fromInteger = NetworkId . Int.fromInteger
  negate = \ (NetworkId a) -> NetworkId (Int.negate a)

instance Enum NetworkId where
  toEnum = NetworkId
  fromEnum = \(NetworkId a) -> a

data OpearationResult where
   SystemEnvResult   :: SystemEnvResult   -> OpearationResult
   TxSubmitionResult :: TxSubmitionResult -> OpearationResult
   deriving (Show)

data TxSubmitionResult where
  Success :: TxId -> TxSubmitionResult
  ErrorTxSubmitionResult :: String -> TxSubmitionResult

instance Show TxSubmitionResult where
  show (Success txId) = "TxSubmitionResult: Success for " ++ show txId
  show (ErrorTxSubmitionResult desc) = "TxSubmitionResult. Error:" ++ desc

data SystemEnvResult where
  SuccessResult        :: SystemEnv -> SystemEnvResult
  ErrorSystemEnvResult :: String -> SystemEnvResult

instance Show SystemEnvResult where
  show (SuccessResult _) = "SuccessResult for SystemEnvResult"
  show (ErrorSystemEnvResult desc) = "ErrorSystemEnvResult for SystemEnv. Error:" ++ desc

data ExtractError = ExtractError String deriving (Show)

instance Exception ExtractError

extractSystemEnv :: (MonadThrow f) => OpearationResult -> f SystemEnv
extractSystemEnv (SystemEnvResult (SuccessResult res)) = pure res
extractSystemEnv _ = throwM (ExtractError "Error with extracting sys env")