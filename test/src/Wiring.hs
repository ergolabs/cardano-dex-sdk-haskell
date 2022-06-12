{-# LANGUAGE OverloadedStrings #-}

module Wiring where

-- import Data.Aeson as Json ( encode )
-- import Utils
-- import qualified ContractDeposit as CD

-- import Control.Monad.Trans.Resource
-- import qualified Data.Text.Encoding      as T
-- import qualified Ledger.Ada                  as P
-- import NetworkAPI.Types
-- import qualified Explorer.Types       as Explorer
-- import qualified Explorer.Models      as Explorer
-- import qualified Explorer.Class       as Explorer
-- import WalletAPI.Utxos
-- import qualified RIO as RIO
-- import SubmitAPI.Service as SubmitAPI
-- import ErgoDex.Amm.PoolActions
-- import WalletAPI.TrustStore as TrustStore
-- import WalletAPI.Vault
-- import NetworkAPI.Service
-- import NetworkAPI.Node.Service

-- import Explorer.Service
-- import Explorer.Config

-- import NetworkAPI.Config.NodeConfig
-- import SubmitAPI.Config

-- import TxSender
-- import WalletAPI.UtxoStore
-- import Ledger.Tx.CardanoAPI
-- import PlutusTx.Builtins.Internal
-- import qualified Cardano.Api         as Crypto
-- import           Data.ByteArray.Encoding (Base(..), convertToBase)
-- import qualified Cardano.Api  as C
-- import qualified Cardano.Api.Byron  as C
-- import qualified Cardano.Api.Shelley  as C
-- import qualified Cardano.Ledger.Alonzo.TxWitness  as C
-- import qualified PlutusTx.Prelude  as PlutusTx
-- import Plutus.V1.Ledger.TxId
-- import qualified Data.ByteString.Base16  as Hex
-- import Plutus.V1.Ledger.Ada
-- import CardanoTx.Models
-- import TokenNames

-- wiring :: IO ()
-- wiring = runResourceT $ do
--   let
--     trustStore     = (mkTrustStore Crypto.AsPaymentKey (SecretFile "/Users/aleksandr/test123/cardano-dex-sdk-haskell/test/ts.json")) :: TrustStore IO Crypto.PaymentKey
--   -- RIO.lift $ TrustStore.init trustStore $ KeyPass ""

--   let
--     explorer       = (mkExplorer $ ExplorerConfig (Uri "0.0.0.0")) :: Explorer IO
--   store            <- RIO.lift $ ((mkUtxoStore) :: IO (UtxoStore IO))
--   let
--     vault          = mkVault trustStore (KeyPass "secret")
--   walletOutputs  <- RIO.lift $ (WalletAPI.Utxos.mkWalletOutputs' explorer vault :: IO (WalletOutputs IO))
--   pkh <- RIO.lift $ getPaymentKeyHash vault
--   let
--     poolActions           = mkPoolActions CD.wallet3PaymentPubKeyHash
--     (candidate, deposit)  = unsafeFromEither $ (runDeposit poolActions) CD.confirmedDeposit CD.poolTuple
--     epochSlots            = C.CardanoModeParams $ C.EpochSlots 21600
--     networkId             = C.Testnet (C.NetworkMagic 1097911063)
--     sockPath              = SocketPath "/tmp/another.socket"
--     network               = (mkNetwork C.AlonzoEra epochSlots networkId sockPath)
--     changeAddr            = DefaultChangeAddress . ChangeAddress $ walletPubKeyHash
--     submitService         = mkTransactions network networkId walletOutputs vault (TxAssemblyConfig Balance Cover changeAddr)
--     pkred = Explorer.PaymentCred . T.decodeUtf8 . convertToBase Base16 . C.serialiseToRawBytes $ pkh
--     collateral = P.Lovelace $ 10 * 100 `div` 100
-- --  let
-- --    json = Json.encode candidate
--   finilized <- RIO.lift $ finalizeTx submitService $ candidate
-- --  RIO.lift $ print finilized
--   value <- RIO.lift $ (selectUtxosStrict walletOutputs) (P.toValue collateral)
--   RIO.lift $ print (show value)
--   submitted <- RIO.lift $ SubmitAPI.submitTx submitService $ finilized
--   RIO.lift $ print submitted
--   RIO.lift $ pure ()

--
--mkWalletOutputs' :: Explorer IO -> Vault IO -> IO (WalletOutputs IO)
--mkWalletOutputs' explorer Vault{..} =
--  getPaymentKeyHash >>= mkWalletOutputs explorer

-- 2.456371
-- 1.000000