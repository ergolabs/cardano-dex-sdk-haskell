module Utils where

import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex
import PlutusTx.Builtins (toBuiltin)
import RIO

import Ledger.Address (Address(..))
import Plutus.V1.Ledger.Api (CurrencySymbol(..), TokenName(..), Value(..), TxOutRef(..), TxId(..))
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusTx.AssocMap as Map
import qualified Ledger.Ada as Ada

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

unsafeFromMaybe :: (Show a) => Maybe a -> a
unsafeFromMaybe (Nothing)    = Prelude.error ("Err:")
unsafeFromMaybe (Just value) = value

mkByteString :: Text -> ByteString
mkByteString input = unsafeFromEither (Hex.decode . T.encodeUtf8 $ input)

mkTokenName'1 :: Text -> TokenName
mkTokenName'1 input = TokenName (BuiltinByteString . T.encodeUtf8 $ input)

mkTokenNameHex :: Text -> TokenName
mkTokenNameHex input = TokenName $ unsafeFromEither $ fmap toBuiltin ((Hex.decode . T.encodeUtf8) $ input)

mkTokenName' :: Text -> TokenName
mkTokenName' input = TokenName (BuiltinByteString $ mkByteString input)

mkCurrencySymbol' :: Text -> CurrencySymbol
mkCurrencySymbol' input = CurrencySymbol (BuiltinByteString $ mkByteString input)

mkAdaValue' :: Integer -> Value
mkAdaValue' count =
  Value $ Map.fromList [(Ada.adaSymbol, Map.singleton Ada.adaToken count)]

mkTxOutRef' :: Text -> Integer -> TxOutRef
mkTxOutRef' hash index = TxOutRef (TxId (BuiltinByteString $ mkByteString hash)) index

mkTokenValue' :: CurrencySymbol -> TokenName -> Integer -> Value
mkTokenValue' cs tn amount =
  Value $ Map.fromList [(cs, Map.singleton tn amount)]



