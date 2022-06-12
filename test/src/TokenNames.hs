module TokenNames
  ( currencySymbolName'
  , poolNft'
  , poolX'
  , poolY'
  , poolLq'
  , walletPubKeyHash
  , addr
  , pkHash
  ) where 

import RIO

import Plutus.V1.Ledger.Api (CurrencySymbol(..), TokenName(..), PubKeyHash(..))
import qualified Cardano.Api as CAddr
import qualified Data.ByteString.Base16  as Hex
import Text.Hex (decodeHex)
import CardanoTx.Address
import Ledger.Address (Address(..), StakePubKeyHash(..), PaymentPubKeyHash(..), pubKeyHashAddress)
import Codec.Serialise (deserialise)
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import qualified Ledger                     as P
import qualified Cardano.Api                as C
import qualified Plutus.Contract.CardanoAPI as Interop
import Data.Either.Combinators
import Utils (mkByteString, mkCurrencySymbol', mkTokenName', unsafeFromMaybe)

addr :: Text
addr = "2f69da3ca1ab89a2fbfd9d7cdae9616957b5bf34403001fb73be9291"

pkHash :: PubKeyHash
pkHash = PubKeyHash $ BuiltinByteString $ mkByteString addr

pkHash1 :: PaymentPubKeyHash
pkHash1 = PaymentPubKeyHash $ PubKeyHash $ BuiltinByteString $ mkByteString addr

stakePkh :: StakePubKeyHash
stakePkh = StakePubKeyHash $ PubKeyHash $ BuiltinByteString $ mkByteString addr

readShellyAddress1 :: Text -> Maybe P.Address
readShellyAddress1 text = do
  let hex = unsafeFromMaybe $ decodeHex text
  saddr <- C.deserialiseFromRawBytes (C.AsAddress C.AsShelleyAddr) hex
  rightToMaybe $ Interop.fromCardanoAddress (C.shelleyAddressInEra saddr :: C.AddressInEra C.AlonzoEra)

walletPubKeyHash :: Address
walletPubKeyHash = unsafeFromMaybe $ readShellyAddress1 "00d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234fef0b89559476283e2bb6a5b55e1790c386e98a24e1e1be6c16e304ab"

currencySymbolName' :: CurrencySymbol
currencySymbolName' = mkCurrencySymbol' "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

poolNft' :: TokenName
poolNft' = mkTokenName' "4e46545f546f6b656e5f6e65775f706f6f6c5f320a"

poolX' :: TokenName
poolX' = mkTokenName' "415f546f6b656e5f6e65775f706f6f6c0a"

poolY' :: TokenName
poolY' = mkTokenName' "425f546f6b656e5f6e65775f706f6f6c0a"

poolLq' :: TokenName
poolLq' = mkTokenName' "544f4b454e5f4c505f4e45575f504f4f4c5f320a"