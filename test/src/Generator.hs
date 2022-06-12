{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Generator where

import           ErgoDex.Contracts.Proxy.Typed.Swap
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Proxy.Typed.Deposit
import           ErgoDex.Contracts.Proxy.Order
import           ErgoDex.Contracts.Proxy.OffChain
import           Cardano.Ledger.Alonzo.TxInfo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import qualified Data.ByteString.Short as SBS
import qualified ErgoDex.PValidators as PSc
import           ErgoDex.Contracts.Proxy.Typed.Redeem
import qualified Data.Text.Encoding as LE
import qualified Data.ByteString.Base16  as Hex
import Cardano.Api
import qualified Cardano.Binary as CBOR
import qualified Data.ByteString.Lazy as BSL
import           System.Random
import qualified Data.ByteString.Char8  as C
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex
import CardanoTx.Address

import           PlutusTx.Builtins.Internal
import           ErgoDex.Contracts.Class
import           SubmitAPI.Internal.Transaction
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import qualified Ledger.Address as Addr
import           Control.Monad
import           Ledger
import           Ledger.Value
import           TestRedeemer
import           PlutusTx.Builtins.Internal
import qualified ErgoDex.Contracts.Typed as ECT
import qualified ErgoDex.Contracts.Pool as ECP
import qualified ErgoDex.Contracts.Proxy.Swap as CPS
import qualified ErgoDex.Contracts.Proxy.Deposit as CPD
import qualified ErgoDex.Contracts.Proxy.Redeem as CPR
import           PlutusTx.IsData.Class
import           Data.Functor
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Serialise as Codec
import Text.Hex (encodeHex)
-- import Ledger.Scripts (toCardanoAPIData)
import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import Cardano.Binary
import Data.Aeson as Json ( encode )
import qualified PlutusTx
import Data.Char (chr)
import Data.ByteString as S (ByteString, unpack)
import ErgoDex.Contracts.OffChain
import Codec.Serialise          (serialise )
import Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import Plutus.V1.Ledger.Scripts (unValidatorScript)
import qualified Data.Text       as T
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Ledger.Typed.Scripts.Validators 
import ErgoDex.Contracts.Types
import PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins     as BI
import Utils
import           Plutus.V1.Ledger.Api
import ErgoDex.PContracts.PPool as PP
import qualified ErgoDex.PContracts.PDeposit as PD
import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin (pasInt, pasByteStr)
import PExtra.API
import Plutarch.DataRepr
import Plutarch.Api.V1 (mkMintingPolicy, PMintingPolicy(..), mintingPolicySymbol, mkValidator)
import Plutarch.Api.V1.Value (PCurrencySymbol(..), PValue(..))
import Plutarch.Lift
import Plutarch.Api.V1.Contexts
-- (CurrencySymbol, TokenName)
-- liquidityMintingPolicyInstancePrint :: IO ()
-- liquidityMintingPolicyInstancePrint = print $ 
--   liquidityMintingPolicyInstance (Coin $ AssetClass (CurrencySymbol (BuiltinByteString $ C.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"), TokenName (BuiltinByteString $ C.pack "544f4b454e5f4e46540a")))

-- writeData2 :: IO ()
-- writeData2 =
--   print $ encodeHex $ LBS.toStrict $ serialise $ liquidityMintingPolicyInstance (Coin $ AssetClass (CurrencySymbol (BuiltinByteString $ C.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"),  TokenName (BuiltinByteString $ C.pack "544f4b454e5f4e46540a")))

poolValidatorScript :: PlutusScript PlutusScriptV1
poolValidatorScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript $ PSc.poolValidator

depositValidatorScript :: PlutusScript PlutusScriptV1
depositValidatorScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript $ PSc.depositValidator

mkPoolConfig :: ECT.PoolConfig
mkPoolConfig =
  let
    coinNft = Coin $ AssetClass (CurrencySymbol (BuiltinByteString $ mkByteString "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"), TokenName (BuiltinByteString $ mkByteString "NFT_Token_new_pool"))
    coinA = Coin $ AssetClass (CurrencySymbol (BuiltinByteString $ mkByteString "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"), TokenName (BuiltinByteString $ mkByteString "A_Token_new_pool"))
    coinB = Coin $ AssetClass (CurrencySymbol (BuiltinByteString $ mkByteString "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"), TokenName (BuiltinByteString $ mkByteString "B_Token_new_pool"))
    coinLP = Coin $ AssetClass (CurrencySymbol (BuiltinByteString $ mkByteString "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"), TokenName (BuiltinByteString $ mkByteString "lp_Token_new_pool"))
  in ECT.PoolConfig coinNft coinA coinB coinLP 1

checkIsUnit :: IO ()
checkIsUnit = do
  let ECT.PoolConfig{..} = mkPoolConfig
      value1 = Value $ Map.singleton (CurrencySymbol (BuiltinByteString $mkByteString "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da")) (Map.singleton (TokenName  (BuiltinByteString $ mkByteString "4e46545f546f6b656e5f6e65775f706f6f6c0a")) 0)
      checkResult = isUnit value1 poolNft
  print checkResult

testCheck :: IO ()
testCheck = do
  let
    a = BI.decodeUtf8 $ BI.emptyByteString
  print a

pd :: Datum
pd = Datum $ toBuiltinData $ ((lift mkPoolConfig) :: ECP.PoolConfig)

pdh :: DatumHash
pdh = datumHash pd

generatePoolConfig :: IO Datum
generatePoolConfig = do
  return $ Datum $ toBuiltinData $ ((lift mkPoolConfig) :: ECP.PoolConfig)

generatePoolConfigHash :: IO DatumHash
generatePoolConfigHash = generatePoolConfig <&> datumHash

mkData :: Datum -> LBS.ByteString
mkData = Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

-- fromTo :: LBS.ByteString -> C.ByteString
-- fromTo s = mkByteString $ (map (chr . fromEnum) $ (LBS.unpack s))

-- printPoolActionInit :: IO ()
-- printPoolActionInit = print 
--     $ Json.encode
--     . scriptDataToJson ScriptDataJsonDetailedSchema
--     . fromPlutusData
--     . PlutusTx.toData 
--     $ Init

writeDataDatum2 :: FilePath -> IO ()
writeDataDatum2 file = do
  LBS.writeFile file (Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData 
    $ ((lift mkPoolConfig) :: ECP.PoolConfig) )

-- writeData1 :: FilePath -> IO ()
-- writeData1 file = do
--   LBS.writeFile file (Json.encode
--     . scriptDataToJson ScriptDataJsonDetailedSchema
--     . fromPlutusData
--     . PlutusTx.toData 
--     $ Init)


printRedeemerValue :: IO ()
printRedeemerValue = print
    $ Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
    $ (ECP.Deposit :: ECP.PoolAction)

generateSwapConfig :: IO SwapConfig
generateSwapConfig = do
  feeNumR           <- randomRIO (1, 100)
  exFeePerTokenNumR <- randomRIO (1, 100)
  exFeePerTokenDenR <- randomRIO (1, 100)
  baseR             <- generateCoin
  quoteR            <- generateCoin
  poolNftR          <- generateCoin
  pkHashR           <- generateRandomPkh
  mqaR              <- generateRandomAmount
  pure $ SwapConfig
    { base             = baseR
    , quote            = quoteR
    , poolNft          = poolNftR
    , feeNum           = feeNumR
    , exFeePerTokenNum = exFeePerTokenNumR
    , exFeePerTokenDen = exFeePerTokenDenR
    , rewardPkh        = pkHashR
    , stakePkh         = Nothing
    , minQuoteAmount   = mqaR
    }

generateRedeemConfig :: IO RedeemConfig
generateRedeemConfig = do
  poolNftR          <- generateCoin
  exFeeR            <- generateRandomAmount
  pkHashR           <- generateRandomPkh
  pure $ RedeemConfig
    { poolNft   = poolNftR
    , exFee     = exFeeR
    , rewardPkh = pkHashR
    }

generateDepositConfig :: IO DepositConfig
generateDepositConfig = do
  poolNftR          <- generateCoin
  exFeeR            <- generateRandomAmount
  pkHashR           <- generateRandomPkh
  pure $ DepositConfig
    { poolNft   = poolNftR
    , exFee     = exFeeR
    , rewardPkh = pkHashR
    }

generateCoin :: IO (Coin a)
generateCoin = do
  assetClassR <- generateRandomAssetClass
  pure $ Coin assetClassR

generateRandomAssetClass :: IO AssetClass
generateRandomAssetClass = do
  curSymbolR <- randString
  tokenNameR <- randString
  pure $ (AssetClass (CurrencySymbol (BuiltinByteString $ C.pack curSymbolR), TokenName (BuiltinByteString $ C.pack tokenNameR)))

generateRandomPkh :: IO PubKeyHash
generateRandomPkh = do
  hashR <- randString
  pure $ PubKeyHash (BuiltinByteString $ C.pack hashR)

generateRandomAmount :: IO (Amount a)
generateRandomAmount = do
  intR <- randomRIO (1, 100)
  pure $ Amount intR

randString :: IO String
randString = liftM (take 10 . randomRs ('a','z')) newStdGen

scriptsbs :: SBS.ShortByteString
scriptsbs = serializePlutusScript (unValidatorScript $ validatorScript poolInstance)

scriptHex = SBS.toShort $ unsafeFromEither $ Hex.decode . T.encodeUtf8 $ "5913b9010000332332233223322323232333222323332223233333333222222223233322232333322223232332232333222323332223232332233223232333332222233223322332233223322332222323232322232325335303533300a3333573466e1cd55cea80424000466660a0666aa096eb9d71aba15008375a6ae85401cdd71aba15006375a6ae84d5d1280311a8261a982699ab9c4901035054310004e4992630490043333573466e1cd55cea8012400046601a64646464646464646464646666ae68cdc39aab9d500a480008cccccccccc06ccd40a48c8c8cccd5cd19b8735573aa0049000119810981e1aba15002302e357426ae8940088d4170d4c174cd5ce2481035054310005e49926135573ca00226ea8004d5d0a80519a8148151aba150093335503075ca05e6ae854020ccd540c1d728179aba1500733502904535742a00c66a05266aa0ae09ceb4d5d0a8029919191999ab9a3370e6aae754009200023350233232323333573466e1cd55cea80124000466a05666a088eb4d5d0a80118249aba135744a00446a0c06a60c266ae712401035054310006249926135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502933504475a6ae854008c124d5d09aba250022350603530613357389201035054310006249926135573ca00226ea8004d5d09aba2500223505c35305d3357389201035054310005e49926135573ca00226ea8004d5d0a80219a814bae35742a00666a05266aa0aeeb8158d5d0a801181d9aba135744a00446a0b06a60b266ae712401035054310005a49926135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea002900311810181e9aba135573ca00646666ae68cdc3a801240084603e608e6ae84d55cf280211999ab9a3370ea00690011180f98191aba135573ca00a46666ae68cdc3a80224000460446eb8d5d09aab9e50062350533530543357389201035054310005549926499264984d55cea80089baa001357426ae8940088d4130d4c134cd5ce249035054310004e49926104d13504b35304c3357389201035054350004d4984d55cf280089baa001135744a00226ae8940044d55cf280089baa0012212330010030022001222222222212333333333300100b00a00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026aa06e446666aae7c004940388cd4034c010d5d080118019aba200203423232323333573466e1cd55cea801a4000466600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a00446a06e6a607066ae712401035054310003949926135573ca00226ea8004d5d0a801999aa805bae500a35742a00466a01eeb8d5d09aba25002235033353034335738921035054310003549926135744a00226aae7940044dd50009110919980080200180110009109198008018011000899aa800bae75a224464460046eac004c8004d540c488c8cccd55cf80112804919a80419aa81a18031aab9d5002300535573ca00460086ae8800c0bc4d5d08008891001091091198008020018900089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a801240044a01046a0546a605666ae712401035054310002c499264984d55cea80089baa001121223002003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135744a00446a0486a604a66ae71241035054310002649926135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d4080d4c084cd5ce24810350543100022499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8119a981219ab9c490103505431000254992649926135573aa00226ea8004484888c00c0104488800844888004480048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d406cd4c070cd5ce249035054310001d499264984d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308d4080d4c084cd5ce2481035054310002249926499264992649926135573aa00826aae79400c4d55cf280109aab9e500113754002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024646464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d400920002300a300b357426aae7940188d4044d4c048cd5ce2490350543100013499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500b35300c3357389201035054310000d499264984d55cea80089baa0012122300200321223001003200120011122232323333573466e1cd55cea80124000466aa01a600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926135573ca00226ea8004498480048004888848cccc00401401000c0088004448848cc00400c008448004448c8c00400488cc00cc008008004c8ccc888cc88c8cc88cc88cc88c8ccc888c8c8ccc888cc88c8c8cc88c8cc88c8c8c8c8ccc888ccc888cccccccc88888888cccc8888cc88cc88cc88cc88cc88cc88cc88cc88c8ccccc88888cc88888c8c8c8c8c8c8c8c8d4c13802c888c8c88c8c8c8c8c8c8c8c94cd4c18ccc88d4c09000888888888894cd4d4174ccd54c14448005414094cd4c1c0ccd5cd19b8f00c0010720711350600011505f003210721070501600c1065153353063333573466e1cd4d5555541794c04c48004888888d4d5419c01488d4c0b94c07048004888ccc1800080140112002065064150011533530633357389210c496e76616c696420706f6f6c00064150011064153353062333573466e1cc8cccd54c11448004c8cd412888ccd412800c004008d411c004cd4124888c00cc008004800488cdc0000a400400290001a980c80b111a98118011111111111005240080c80c62a0022a66a60c466ae7124118496e76616c6964206e756d626572206f6620696e707574730006315001106313530245009222353029003223353044002253353067333573466e3c0040401a41a0540205401c9401c54cd4c180cd5ce2491a496e76616c6964207265776172642070726f706f736974696f6e0006115001106115335305f333573466e20ccc1454019221004881000050600611500115335305f3357389211a556e6661697220657865637574696f6e206665652074616b656e0006015001106015335305e333573466e20d4d5555541654c03848004888888d4d5418800888ccc16140340080054cd4c178ccd5cd19b895001500206005f150011500205f060106013357389201164d696e696d616c20726577617264206e6f74206d65740005f13370666e08d4d5416d4008880094028d4c151402c88800c4cdc199b823535505a5001220015009353053500a22200213535555550565300b12001222222533530613305e0045057133550603370266e04d4d5555541714c04448004888888d4d5419401088ccc16d405400800402c01cd4d5555541714c04448004888888d4d5419400c88ccc16d40540080044d4d5555541714c044480048888894cd4c19ccc19000d41744cd54198d4d5555541894c05c48004888888d4d541ac01088ccc185406c008004cdc099b8135355555506253017120012222223535506b00322333061501b00200101100d1335506635355555506253017120012222223535506b00422333061501b00200135355555506253017120012222223535506b00322333061501b002001135301c5001222002135300f00c22353019002222222222253353505233355304612001504523530260012235302a00122235302f00322335304a00225335306d333573466e3c0040581bc1b84d4174024541709417002884d4c098004880044d4c094d412d2622001153353504235300b008223530150022222222222533335301900b215050215050215050213335530431200150422353023001225335306453353064333573466e3cd4c0ac00888008d4c0ac010880081981944ccd5cd19b8735302b0022200135302b0042200106606510651350540031505300b213530160012235301a00122200213530153503b49888d4c0640048880084d4c12940048880044d4d5555541354c00848004888888d4c10001888888ccc154cc1394030010cc1394030010cdc0a41fdfffffffffffffffe026609ca01800426a6028a6004240024440046a004440026a0024400464664646460020024466006600400400244246a6008246a60080066a0060020024246600246a00644600400646a00644600200600224446600642646a601400e446a60280044444444444a66a6a09a666aa608224002a0804a66a60c0666ae68cdc39a9aa82f007111a9811801911a981380091119982c801003803240040c40c226a0a00022a09e014426a60420024400226a08c6a609666ae71240114706f6f6c20696e707574206e6f7420666f756e640004c498d4d5555541354c0084800488888801484d4c0554c00c480048894cd4d410c00484d4c0f8cc051402400488888cccccd55555415ccc065403801801401000c0080044d4c0f4d40f0d4c104cd5ce24811f706f6f6c20696e70757420646174756d2068617368206e6f7420666f756e640004249888888cccccd555554158d4104d4c118cd5ce24811f706f6f6c20696e70757420646174756d2068617368206e6f7420666f756e640004749801401000c0080044d4c010004880088848cc00400c0088004848888c010014848888c00c014848888c008014848888c0040148004894cd4d40c8d4c00c00888888888894cd4d40f0ccd54c0c04800540bc8d4d54134004894cd4c144ccd5cd19b8f00200e05305213504100315040002213503f3535504d001220011503d232323232323215335350393333333574800e46666ae68cdc39aab9d5007480008cccd55cfa8039281e91999aab9f50072503e233335573ea00e4a07e46666aae7d401c941008cccd55cfa8039282091999aab9f35744a0104a66a6a082666aa07c07a07a6ae854034854cd4d4108ccd540fc0f80f8d5d0a80690a99a9a821999aa82001f81f9aba1500d215335350443335504104004035742a01a42a66a6a08a646666666ae900049412094120941208d4124dd6801128240229aba1500d21350483333304100500400300200115046150451504415043150422504203f03e03d03c03b03a2503c498940ec940ec940ec940ec0e0840044d40c8d4c0dccd5ce249186572726f72206465636f64696e6720706f6f6c2064617461000384984d5d1280089aba25001135744a00226ae8940044d55cf280089baa00113502b3530303357389211a706f6f6c20696e70757420646174756d206e6f7420666f756e6400031498888888888848cccccccccc00402c02802402001c01801401000c00880048848cc00400c008800488848ccc00401000c00880048848cc00400c00880048848cc00400c008800448848cc00400c0084800448848cc00400c0084800448848cc00400c00848004484888c00c010448880084488800448004848888888c01c0208848888888cc018024020848888888c014020488888880104888888800c8848888888cc0080240208848888888cc00402402080048488c00800c888488ccc00401401000c80048488c00800c8488c00400c80044cd4014894cd4d403c0088400c40054038c8004d5408488448894cd4d40580044d401800c884ccd4024014c010008ccd54c01c4800401401000448d4d400c0048800448d4d40080048800848848cc00400c008480044988888848ccccc00401801401000c00880048ccccccd5d20009280392803928039280391a8041bae002004112223232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021280691999aab9f50042500e233335573e6ae89401494cd4d4038c02cd5d0a80390a99a9a80798059aba1500721350123355021002001150101500f2500f00c00b00a2500c4989402c9402c9402c9402c0204d5d1280089aab9e500113754002240024002242446004006224400224002446a6aa01e0024466600a0080040024446464600200a640026aa02a4466a6a00e0029000111a9aa80a00111299a980c199ab9a3371e0040120340322600e0022600c006640026aa0284466a6a00c0029000111a9aa80980111299a980b999ab9a3371e00400e03203020022600c00622440042442446600200800624002266aa01291010048810022212333001004003002200122221233330010050040030022001111111222222123333330010070060050040030021111112001223535500300222353550050032253353009333573466e3c01000802c0284ccd5cd19b8f00300100b00a100a1122123300100300211200112200212200120011123230010012233003300200200101"

txCtx :: Data
txCtx = unsafeFromEither $ decodeFullDecoder "test" Codec.decode (BSL.fromStrict $ unsafeFromEither $ Hex.decode . T.encodeUtf8 $ "d8799fd8799f9fd8799fd8799fd8799f58209fd58b3004817d311abad68724729f2bf9de907fff3699056cfd3ccc8c6db198ff00ffd8799fd8799fd87a9f581ce9c53d2119d2efd1dd1e628e8f8a8efd70732bb85d16293c45df8cedffd87a80ffa240a1401a00499a63581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487daa251415f546f6b656e5f6e65775f706f6f6c0a0a51425f546f6b656e5f6e65775f706f6f6c0a0ad8799f5820e95a700acbc970664fb869ec5d1e6b754648b1de76252c321c68bbea59f52a75ffffffd8799fd8799fd8799f5820b48e7cfb1dbc6f7e686a29fd0593eb04c2ef8a04cff9a65025b8216a33151425ff01ffd8799fd8799fd87a9f581c44ff647262c8cb1fec256c674fb5dcca3060c1a1413a3a9d3517176effd87a80ffa240a1401a002dc6c0581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487daa451415f546f6b656e5f6e65775f706f6f6c0a0a51425f546f6b656e5f6e65775f706f6f6c0a0a554e46545f546f6b656e5f6e65775f706f6f6c5f320a0154544f4b454e5f4c505f4e45575f504f4f4c5f320a1b7ffffffffffffff5d8799f5820068868da6d4f3dc0212aef56e1278a5fde10972330ce434e7883c3680145afacffffffff9fd8799fd8799fd87a9f581c44ff647262c8cb1fec256c674fb5dcca3060c1a1413a3a9d3517176effd87a80ffa240a1401a002dc6c0581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487daa451415f546f6b656e5f6e65775f706f6f6c0a1451425f546f6b656e5f6e65775f706f6f6c0a14554e46545f546f6b656e5f6e65775f706f6f6c5f320a0154544f4b454e5f4c505f4e45575f504f4f4c5f320a1b7fffffffffffffebd8799f5820068868da6d4f3dc0212aef56e1278a5fde10972330ce434e7883c3680145afacffffd8799fd8799fd8799f581c2f69da3ca1ab89a2fbfd9d7cdae9616957b5bf34403001fb73be9291ffd87a80ffa240a1401a002b15e3581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487daa154544f4b454e5f4c505f4e45575f504f4f4c5f320a0ad87a80ffffa140a14000a140a140008080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff809fd8799f5820068868da6d4f3dc0212aef56e1278a5fde10972330ce434e7883c3680145afacd8799fd8799f581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da554e46545f546f6b656e5f6e65775f706f6f6c5f320affd8799f581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da51415f546f6b656e5f6e65775f706f6f6c0affd8799f581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da51425f546f6b656e5f6e65775f706f6f6c0affd8799f581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da54544f4b454e5f4c505f4e45575f504f4f4c5f320aff1903e8ffffd8799f5820e95a700acbc970664fb869ec5d1e6b754648b1de76252c321c68bbea59f52a75d8799fd8799f581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da554e46545f546f6b656e5f6e65775f706f6f6c5f320aff1a001e8480581c2f69da3ca1ab89a2fbfd9d7cdae9616957b5bf34403001fb73be92911a001e8480ffffffd8799f582035a7463b515cc2f6becd47ce361ba6d53420424a394782428da9c861839d7795ffffd87a9fd8799fd8799f58209fd58b3004817d311abad68724729f2bf9de907fff3699056cfd3ccc8c6db198ff00ffffff")

datumD :: Data
datumD = unsafeFromEither $ decodeFullDecoder "test" Codec.decode (BSL.fromStrict $ unsafeFromEither $ Hex.decode . T.encodeUtf8 $ "d8799fd8799f581c805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da554e46545f546f6b656e5f6e65775f706f6f6c5f320aff1a001e8480581c2f69da3ca1ab89a2fbfd9d7cdae9616957b5bf34403001fb73be92911a001e8480ff")

redeemerD :: Data
redeemerD = unsafeFromEither $ decodeFullDecoder "test" Codec.decode (BSL.fromStrict $ unsafeFromEither $ Hex.decode . T.encodeUtf8 $ "d87980")

txCtxPlutus = (fromData txCtx) :: Maybe PV1.ScriptContext

scriptData = do
  let
    a = defaultCostModelParams
    poolDatum = datumD
    redeemer = redeemerD
    maxBudget = transExUnits (ExUnits 16000000 1000000000)

  b <- fmap (\m -> PV1.evaluateScriptRestricting Verbose m maxBudget scriptHex [poolDatum, redeemer]) a
  return b

testPubKeyHashReward :: PubKeyHash
testPubKeyHashReward = "f62ccc5df2270b4450e55e39bb60cc0b19ef6498ac280ce854a3daa4"

--wallet3
pPubKeyHashReward :: PubKeyHash
pPubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

pPubKeyHashRewardNewWallet :: PubKeyHash
pPubKeyHashRewardNewWallet = "deff3293b2e7684b3144f75ebabd1b876c7e83630d5fbbb171f6b119"

poolValidatorP = PSc.poolValidator

poolValidatorM = unsafeMkTypedValidator $ PSc.poolValidator

depositValidatorM = unsafeMkTypedValidator $ PSc.depositValidator

swapValidatorM = unsafeMkTypedValidator $ PSc.swapValidator

redeemValidatorM = unsafeMkTypedValidator $ PSc.redeemValidator

newPoolAddress = renderToShellyAddress (Testnet (NetworkMagic 1097911063)) poolValidatorM

poolAddr = scriptAddress PSc.poolValidator

newDepositAddress = renderToShellyAddress (Testnet (NetworkMagic 1097911063)) depositValidatorM
depositAddressC = scriptAddress PSc.depositValidator
newSwapAddress = renderToShellyAddress (Testnet (NetworkMagic 1097911063)) swapValidatorM
swapAddressC = scriptAddress PSc.swapValidator
newRedeemAddress = renderToShellyAddress (Testnet (NetworkMagic 1097911063)) redeemValidatorM
redeemAddressC = scriptAddress PSc.redeemValidator

newPoolConfig =
  ECP.PoolConfig
    { poolNft = AssetClass (currencySymbolName, nftTn)
    , poolX = AssetClass (currencySymbolName, xTn)
    , poolY = AssetClass (currencySymbolName, yTn)
    , poolLq = AssetClass (currencySymbolName, lpTn)
    , poolFeeNum = 995
    }

test = PlutusTx.toData $ (ECP.PoolRedeemer
                           { action = ECP.Deposit
                           , selfIx = 0
                           })

poolRedeemer :: Redeemer
poolRedeemer = Redeemer . toBuiltinData $ TestRedeemer 0 0

poolConfigDatum :: Datum
poolConfigDatum = Datum . toBuiltinData $ newPoolConfig

poolConfigDatumHash :: DatumHash
poolConfigDatumHash = Ledger.datumHash poolConfigDatum

poolConfigJson = (Json.encode
                     . scriptDataToJson ScriptDataJsonDetailedSchema
                     . fromPlutusData
                     . PlutusTx.toData $ poolConfigDatum)

poolConfigRaw = T.decodeUtf8 . Hex.encode . BSL.toStrict . Codec.serialise $ poolConfigDatum

someDatum = fmap BSL.fromStrict (Hex.decode $ (T.encodeUtf8 $ "d8799fd8799f4040ffd8799f581c8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa444d494c4bff1a01a07bec00d87a80ff"))

poolConfigCbor = (Json.encode
                     . scriptDataToJson ScriptDataJsonDetailedSchema
                     . fromPlutusData
                     . PlutusTx.toData $ poolConfigDatum)

newDepositConfig =
  CPD.DepositConfig
    { poolNft       = AssetClass (currencySymbolName, nftTn)
    , tokenA        = AssetClass (currencySymbolName, xTn)
    , tokenB        = AssetClass (currencySymbolName, yTn)
    , tokenLp       = AssetClass (currencySymbolName, lpTn)
    , exFee         = 2
    , rewardPkh     = pPubKeyHashReward
    , stakePkh         = Nothing
    , collateralAda = 1
    }

newRedeemConfig =
  CPR.RedeemConfig
    { poolNft   = AssetClass (currencySymbolName, nftTn)
    , poolX     = AssetClass (currencySymbolName, xTn)
    , poolY     = AssetClass (currencySymbolName, yTn)
    , poolLp    = AssetClass (currencySymbolName, lpTn)
    , exFee     = 2
    , rewardPkh = pPubKeyHashReward
    , stakePkh  = Nothing
    }

newRedeemConfigDatum :: Datum
newRedeemConfigDatum = Datum . toBuiltinData $ newRedeemConfig

newRedeemConfigDatumHash :: DatumHash
newRedeemConfigDatumHash = Ledger.datumHash newRedeemConfigDatum

newRedeemConfigDatumJson = (Json.encode
                              . scriptDataToJson ScriptDataJsonDetailedSchema
                              . fromPlutusData
                              . PlutusTx.toData $ newRedeemConfigDatum)

newRedeemConfigDatumHex = (T.decodeUtf8 . Hex.encode $ (LBS.toStrict $ CBOR.serialize $ PlutusTx.toData $ newRedeemConfigDatum))


newSwapConfig =
  CPS.SwapConfig
    { base          = AssetClass (currencySymbolName, xTn)
    , quote         = AssetClass (currencySymbolName, yTn)
    , poolNft       = AssetClass (currencySymbolName, nftTn)
    , feeNum = 995
    , exFeePerTokenNum = 2500000
    , exFeePerTokenDen = 1
    , rewardPkh       = pPubKeyHashReward
    , stakePkh         = Nothing
    , baseAmount     = 2
    , minQuoteAmount = 1
    }

newSwapConfigDatum :: Datum
newSwapConfigDatum = Datum . toBuiltinData $ newSwapConfig

newSwapConfigDatumHash :: DatumHash
newSwapConfigDatumHash = Ledger.datumHash newSwapConfigDatum

newSwapConfigDatumJson = (Json.encode
                              . scriptDataToJson ScriptDataJsonDetailedSchema
                              . fromPlutusData
                              . PlutusTx.toData $ newSwapConfigDatum)

newSwapConfigDatumHex = (T.decodeUtf8 . Hex.encode $ (LBS.toStrict $ CBOR.serialize $ PlutusTx.toData $ newSwapConfigDatum))


newDepositConfigDatum :: Datum
newDepositConfigDatum = Datum . toBuiltinData $ newDepositConfig

orderRedeemer :: Redeemer
orderRedeemer = Redeemer . toBuiltinData $ OrderRedeemer 0 1 0 Apply

newDepositConfigDatumHash :: DatumHash
newDepositConfigDatumHash = Ledger.datumHash newDepositConfigDatum

newDepositConfigDatumJson = (Json.encode
                              . scriptDataToJson ScriptDataJsonDetailedSchema
                              . fromPlutusData
                              . PlutusTx.toData $ newDepositConfigDatum)

newDepositConfigDatumHex = (T.decodeUtf8 . Hex.encode $ (LBS.toStrict $ CBOR.serialize $ PlutusTx.toData $ newDepositConfigDatum))

newDepositRedeemerJson = (Json.encode
                              . scriptDataToJson ScriptDataJsonDetailedSchema
                              . fromPlutusData
                              . PlutusTx.toData $ poolRedeemer)

newOrderRedeemerJson = (Json.encode
                              . scriptDataToJson ScriptDataJsonDetailedSchema
                              . fromPlutusData
                              . PlutusTx.toData $ orderRedeemer)

currencySymbolName :: CurrencySymbol
currencySymbolName = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

currencySymbolNameX :: CurrencySymbol
currencySymbolNameX = "78"

currencySymbolNameY :: CurrencySymbol
currencySymbolNameY = "79"

currencySymbolNameNft :: CurrencySymbol
currencySymbolNameNft = "6e6674"

currencySymbolNameLq :: CurrencySymbol
currencySymbolNameLq = "6c71"

xTn :: TokenName
xTn = "ergoTestTokenA"

yTn :: TokenName
yTn = "ergoTestTokenB"

nftTn :: TokenName
nftTn = "321ergoTestNFT321"

lpTn :: TokenName
lpTn = "321ergoTestTokenLP321"
