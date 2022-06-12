module TxSender where

import qualified ErgoDex.Amm.PoolActions as PA
import qualified ErgoDex.Amm.PoolSetup as PS
-- import qualified Data.ByteString.Char8  as C
import Plutus.V1.Ledger.Api
import PlutusTx.Builtins.Internal
import qualified Ledger.Ada   as Ada
import PlutusTx.AssocMap as Map
import Plutus.V1.Ledger.Address
-- import Ledger.Typed.Scripts.Validators 
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Tx
-- import Ledger.Typed.Scripts.Validators (validatorAddress)
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex
import ErgoDex.Amm.Scripts
import Codec.Serialise          (serialise )
import RIO as RIO
import ErgoDex.Amm.Orders
import           ErgoDex.State
import           CardanoTx.Models
import qualified Ledger as Ledger
import ErgoDex.Contracts.OffChain
import ErgoDex.Contracts.Types
import ErgoDex.Types
import ErgoDex.Amm.Pool
import ErgoDex.Contracts.Pool  as CPool (PoolConfig(..))
import Utils
import Generator
import           ErgoDex.Contracts.Pool
import Ledger.Typed.Scripts.Validators 

--mkPoolActions :: PA.PoolActions
--mkPoolActions = PA.mkPoolActions $ PubKeyHash (BuiltinByteString $ mkByteString "Init")

-- mkConfirmedDeposit :: Confirmed Deposit
-- mkConfirmedDeposit = Confirmed mkConfirmedDepositPoolTxOut mkDeposite


-- ------------------------------------------------------------

mkLpValue :: [(CurrencySymbol, Map.Map TokenName Integer)]
mkLpValue =
    let
        currensySymbolLp = mkCurrencySymbol "97d96c529c5f6e74515f79d07dff6229aa989fef9114befed45e40a4"
        tokenNameLp = mkTokenName "544f4b454e5f4c500a"
        lpMap = Map.singleton tokenNameLp 20
    in [(currensySymbolLp, lpMap)]

mkValue :: Text -> Text -> Integer -> Value
mkValue cs tn i =
    let
        currensySymbolNft = mkCurrencySymbol cs
        tokenNameNft = mkTokenName tn
        nftMap = Map.singleton tokenNameNft i
    in Value $ Map.fromList [(currensySymbolNft, nftMap)]

mkAdaValue' :: Integer -> [(CurrencySymbol, Map.Map TokenName Integer)]
mkAdaValue' i =
    let
        currensySymbolAda = Ada.adaSymbol
        tokenNameAda = Ada.adaToken
        adaMap = Map.singleton tokenNameAda i
    in [(currensySymbolAda, adaMap)]

mkNftValue :: [(CurrencySymbol, Map.Map TokenName Integer)]
mkNftValue =
    let
        currensySymbolNft = mkCurrencySymbol "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"
        tokenNameNft = mkTokenName "4e46545f546f6b656e5f6e65775f706f6f6c0a"
        nftMap = Map.singleton tokenNameNft 1
    in [(currensySymbolNft, nftMap)]

constantAdaValue :: Value
constantAdaValue =
  Value $ Map.singleton Ada.adaSymbol (Map.singleton Ada.adaToken 1758875)

mkAdaValue :: [(CurrencySymbol, Map.Map TokenName Integer)]
mkAdaValue =
    let
        currensySymbolAda = Ada.adaSymbol
        tokenNameAda = Ada.adaToken
        adaMap = Map.singleton tokenNameAda 78472201
    in [(currensySymbolAda, adaMap)]

mkInitPoolValue :: Value
mkInitPoolValue =
    let
      currensySymbolTokens = mkCurrencySymbol "tokens"
      tokenNameA = mkTokenName "A_Token_new_pool"
      tokenNameB = mkTokenName "B_Token_new_pool"
      tokenNameNFT = mkTokenName "lp_Token_new_pool"

      tokensMap = Map.fromList $ (tokenNameA, 10) : (tokenNameB, 10) : (tokenNameNFT, 1) : []

      tokenCSMap = [(currensySymbolTokens, tokensMap)]

    in Value $ Map.fromList (tokenCSMap <> mkAdaValue)
--
--mkConfirmedDepositPoolTxOut :: FullTxOut
--mkConfirmedDepositPoolTxOut =
--    FullTxOut
--        { fullTxOutRef = (mkTxOutRef "poolTxId" 1)
--        , fullTxOutAddress = scriptHashAddress poolValidatorHash
--        , fullTxOutValue = undefined
--        , fullTxOutDatumHash = Just pdh
--        , fullTxOutDatum = Just pd
--        }

-- mkDeposite :: Deposit
-- mkDeposite =
--     Deposit
--         { depositPoolId = PoolId $ Coin $ mkAssetClass "nft" "nft"
--         , depositPair = (mkAssetEntry (mkAssetClass "token_a" "token_a") 5, mkAssetEntry (mkAssetClass "token_b" "token_b") 10)
--         , depositExFee = ExFee $ Amount 10000
--         , depositRewardPkh = PubKeyHash (BuiltinByteString $ mkByteString "key") -- ???
--         }

-- --------------------------------------------------------------------------------
-- UTILS
-- --------------------------------------------------------------------------------

poolValidatorHash :: ValidatorHash
poolValidatorHash = validatorHash poolInstance

mkTxOutRef :: Text -> Integer -> TxOutRef
mkTxOutRef input index = TxOutRef (TxId (BuiltinByteString $ mkByteString input)) index

mkTxId :: Text -> TxId
mkTxId input = TxId (BuiltinByteString $ mkByteString input)

mkTxId' :: Text -> TxId
mkTxId' input =
    TxId $ BuiltinByteString $ unsafeFromEither (Hex.decode . T.encodeUtf8 $ input)

mkCurrencySymbol :: Text -> CurrencySymbol
mkCurrencySymbol input = CurrencySymbol (BuiltinByteString $ mkByteString input)

mkTokenName :: Text -> TokenName
mkTokenName input = TokenName (BuiltinByteString $ mkByteString input)

mkAssetClass :: Text -> Text -> AssetClass
mkAssetClass cs tn = AssetClass ((mkCurrencySymbol cs), (mkTokenName tn))

mkAssetEntry :: AssetClass -> Integer -> AssetEntry
mkAssetEntry ac i = AssetEntry (ac, i)


