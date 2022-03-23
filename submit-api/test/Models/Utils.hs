module Models.Utils
  ( genNft
  , genNftWrong
  , genX
  , genY
  , genLQ
  , genCS
  , genTxOutRef
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

import PlutusTx.Builtins.Internal (BuiltinByteString(..))

import qualified System.Random as Random
import System.IO.Unsafe
import Control.Monad

import Plutus.V1.Ledger.Api

data BSGenerator = BSGenerator
  { genByteString :: BS.ByteString
  }

genNftWrong :: BS.ByteString
genNftWrong = mkByteString $ T.pack "4e46545f546f6b656e5f6e65775f706f6f6c0a4e4e4e"

genNft :: BS.ByteString
genNft = mkByteString $ T.pack "4e46545f546f6b656e5f6e65775f706f6f6c0a"

genX :: BS.ByteString
genX = mkByteString $ T.pack "415f546f6b656e5f6e65775f706f6f6c0a"

genY :: BS.ByteString
genY = mkByteString $ T.pack "425f546f6b656e5f6e65775f706f6f6c0a"

genLQ :: BS.ByteString
genLQ = mkByteString $ T.pack "6572676f6c6162736c70746f6b656e"

genCS :: BS.ByteString
genCS = mkByteString $ T.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

genTxOutRef :: TxOutRef
genTxOutRef = TxOutRef (TxId (BuiltinByteString $ mkByteString $ T.pack "bcdff96396996cfdc4a6a77a0b973b3d755d84eba806e0bd0f99218a87a1a841")) 0

initBSGenerator :: BSGenerator
initBSGenerator =
  let gen = unsafePerformIO (Random.getStdGen :: IO Random.StdGen)
  in BSGenerator (genByteString' gen)

genRandomString :: Random.StdGen -> Int -> String
genRandomString gen length = (take length . Random.randomRs ('a','z')) gen

genRandomText :: Random.StdGen -> Int -> T.Text
genRandomText gen length = T.pack (genRandomString gen length)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

genByteString' :: Random.StdGen -> BS.ByteString
genByteString' gen = E.encodeUtf8 (genRandomText gen (fromIntegral 32))

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)