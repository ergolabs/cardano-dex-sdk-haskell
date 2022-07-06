module NetworkAPI.Service
  ( NodeError(..)
  , CardanoNetwork(..)
  , mkCardanoNetwork
  ) where

import RIO

import qualified Data.Text as Text

import System.Logging.Hlog (Logging(..), MakeLogging(..))

import           Cardano.Api
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import NetworkAPI.Types (SystemEnv(..))
import NetworkAPI.PoolsConnector (PoolsConnector(PoolsConnector, runWithPoolConnection, runAsyncWithPoolConnection, runOnRandomConnection))
import ErgoDex.Amm.Pool (Pool)
import NetworkAPI.Node.Types (SystemEnvResult (SuccessResult), OpearationResult (SystemEnvResult, TxSubmitionResult), TxSubmitionResult (Success), extractSystemEnv)
import Data.Aeson (Value(String))

data NodeError
  = NodeConnectionFailed
  | EraMismatch
  | WrongMode
  | TxSubmissionFailed Text
  deriving (Show, Exception)

data CardanoNetwork f era = CardanoNetwork
  { getSystemEnv :: f SystemEnv
  , submitTx     :: Pool -> Tx era -> f ()
  }

mkCardanoNetwork
  :: (MonadIO i, MonadThrow f, MonadUnliftIO f)
  => MakeLogging i f
  -> CardanoEra era
  -> PoolsConnector f
  -> i (CardanoNetwork f era)
mkCardanoNetwork MakeLogging{..} cera PoolsConnector{..} = do
  logging   <- forComponent "CardanoNetwork"
  emptyMVar <- newMVar Nothing
  pure $ CardanoNetwork
    { getSystemEnv = withAsyncCache logging emptyMVar (runOnRandomConnection (getSystemEnv' cera) >>= extractSystemEnv)
    , submitTx     = \pool tx -> runWithPoolConnection pool (\conn -> submitTx' logging cera conn tx) >> pure ()
    }

getSystemEnv'
  :: (MonadIO f, MonadThrow f)
 => CardanoEra era
  -> LocalNodeConnectInfo CardanoMode
  -> f OpearationResult
getSystemEnv' era conn =
  case (cardanoEraStyle era, toEraInMode era CardanoMode) of
    (ShelleyBasedEra sbe, Just eInMode) ->
      either (const $ throwM EraMismatch) pure =<<
        either (const $ throwM NodeConnectionFailed) pure =<<
          liftIO (executeLocalStateQueryExpr conn Nothing $ \_ntcVersion -> do
            pparams     <- queryExpr $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
            eraHistory  <- queryExpr $ QueryEraHistory CardanoModeIsMultiEra
            systemStart <- queryExpr QuerySystemStart
            stakePools  <- queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

            pure $ do
              pparams' <- pparams
              pools'   <- stakePools
              pure $ SystemEnvResult $ SuccessResult $ SystemEnv pparams' systemStart pools' eraHistory
          )
    _ -> throwM WrongMode

submitTx'
  :: (MonadIO f, MonadThrow f)
  => Logging f
  -> CardanoEra era
  -> LocalNodeConnectInfo CardanoMode
  -> Tx era
  -> f OpearationResult
submitTx' Logging{..} era conn tx =
  case toEraInMode era CardanoMode of
    Just eraInMode -> do
      let txInMode = TxInMode tx eraInMode
      _ <- infoM ("Going send tx with id" ++ show (getTxId . getTxBody $ tx))
      res <- liftIO $ submitTxToNodeLocal conn txInMode
      case res of
        Net.Tx.SubmitSuccess     -> pure $ TxSubmitionResult (Success (getTxId . getTxBody $ tx)) --infoM @String "Transaction successfully submitted."
        Net.Tx.SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _ -> throwM $ TxSubmissionFailed $ Text.pack $ show err
            TxValidationEraMismatch _     -> throwM EraMismatch
    _ -> throwM WrongMode

withAsyncCache
   :: (MonadIO f, MonadUnliftIO f, Show a)
   => Logging f
   -> MVar (Maybe a)
   -> f a
   -> f a
withAsyncCache Logging{..} mVar fa = do
  mVarReadResult <- readMVar mVar
  case mVarReadResult of
    Nothing -> do
      _  <- infoM @String "Async cache is empty. Going to retrive data"
      a  <- fa
      _  <- infoM @String ("Going to put in cache. " ++ show a)
      _  <- swapMVar mVar (Just a)
      return a
    Just env -> infoM @String ("Cache is not empty. " ++ show env) >> async (updateAsyncCache' mVar fa) >> pure env

updateAsyncCache'
  :: (MonadIO f, MonadUnliftIO f)
  => MVar (Maybe a)
  -> f a
  -> f ()
updateAsyncCache' mVar fa = fa >>= (void . swapMVar mVar . Just)

--todo: only for test