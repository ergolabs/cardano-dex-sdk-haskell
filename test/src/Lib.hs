{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where

import Generator
import Text.Hex (encodeHex)
import CardanoTx.Address
import WalletAPI.TrustStore
import qualified ContractDeposit as CD
import qualified Cardano.Api         as Crypto
import TokenNames
import Cardano.Api (writeFileTextEnvelope, Error(displayError))

runApp :: IO ()
runApp = do
  -- checkIsUnit
  -- d  <- generatePoolConfig
  -- dh <- generatePoolConfigHash
--  _ <- print test
--  ts <- importTrustStoreFromCardano Crypto.AsPaymentKey (SecretFile "/Users/aleksandr/test123/cardano-dex-sdk-haskell/test/ts.json") "/Users/aleksandr/test123/cardano-dex-sdk-haskell/test/src/wallet3.skey" (KeyPass "secret")
  -- _ <- (WalletAPI.TrustStore.init ts) (KeyPass "secret")
  -- print someDatum
  _ <- print (readShellyAddress "addr_test1qrmzenza7gnsk3zsu40rnwmqes93nmmynzkzsr8g2j3a4fzvqenn455uh4r4e740drhycwlssxgnam4tjwf7uqfta4xqjhwx2c")
  _ <- print (readShellyAddress "addr_test1vrmzenza7gnsk3zsu40rnwmqes93nmmynzkzsr8g2j3a4fqdg2mex")
  _ <- print "============="
  _ <- print poolAddr
  _ <- print newPoolAddress
  _ <- print poolConfigDatumHash
--  _ <- print poolConfigJson
  _ <- print poolConfigRaw
  _ <- print "============="
  _ <- print newDepositAddress
  _ <- print depositAddressC
  _ <- print "============="
  _ <- print newSwapAddress
  _ <- print swapAddressC
  _ <- print "============="
  _ <- print newRedeemAddress
  _ <- print redeemAddressC
  pure ()
--  _ <- print redeemAddressC
--  _ <- print newDepositConfigDatumHash
--  _ <- print newDepositConfigDatumJson
--  result  <- writeFileTextEnvelope "/Users/aleksandr/IdeaProjects/instanceGenerator/pool.plutus" Nothing poolValidatorScript
--  _ <- print newDepositRedeemerJson
--  _ <- print "----------------------"
--  _ <- print newDepositConfigDatumHash
--  _ <- print "----------------------"
--  result2 <- writeFileTextEnvelope "/Users/aleksandr/IdeaProjects/instanceGenerator/deposit.plutus" Nothing depositValidatorScript
--  _ <- print newDepositConfigDatumJson
--  _ <- print "----------------------"
--  print orderRedeemer
--  testCheck
--  print txCtx
--  printParams
--    print $ show scriptData
--  print $ show scriptData
  -- print d
  -- printContract
  --writeContract "/Users/aleksandr/IdeaProjects/instanceGenerator/dex.uplc"
--  writeData "/home/timofey/development/haskell/instanceGenerator/dex.json"
--  writeDataDatum2 "/home/timofey/development/haskell/instanceGenerator/init_pool_datum.json"
  -- writeData1 "/home/timofey/development/haskell/instanceGenerator/dex1.json"

