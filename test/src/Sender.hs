module Sender where

import qualified CardanoTx.Models as Sdk
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Scripts
import qualified Data.ByteString.Char8  as C
import Generator
import qualified Ledger.Ada   as Ada
import           Ledger.Value (AssetClass, assetClass)
import Plutus.V1.Ledger.Tx

poolValidatorHash :: ValidatorHash
poolValidatorHash = ValidatorHash $ serialise $ getValidator $ validatorScript poolInstance

mkTxCandidate :: Sdk.TxCandidate
mkTxCandidate =
    TxCandidate
        { Set.Set adaFullTxIn
        , [mkTxOutCandidate]
        , MintValue mempty
        , mempty
        , ReturnTo Address
        , Interval (10) (20)}

adaFullTxIn :: Sdk.FullTxIn
adaFullTxIn =
  FullTxIn
    { adaTxInOut
    , ConsumePublicKeyAddress
    }

adaTxOut :: Sdk.FullTxOut
adaTxOut =
  FullTxOut 
    { TxOutRef {(TxId "id") , 1}
    , pubKeyHashAddress "address"
    , Value
        { Map.singleton Ada.adaSymbol (Map.singleton Ada.adaToken 10000)
        }
    , Nothing
    , Nothing
    }

--   FullTxOut 
--     { TxOutRef {(TxId "id") , 1}
--     , scriptHashAddress poolValidatorHash
--     , Value
--         { Map.singleton Ada.adaSymbol (Map.singleton Ada.adaToken 10000)
--         }
--     , Just pdh
--     , Just pd
--     }

mkTxOutCandidate :: TxOutCandidate
mkTxOutCandidate =
    TxOutCandidate
        { scriptHashAddress poolValidatorHash
        , Value
            { Map.singleton Ada.adaSymbol (Map.singleton Ada.adaToken 1000)
        , Just pd
        }

asCurrencySymbol :: String -> CurrencySymbol
asCurrencySymbol = CurrencySymbol $ BuiltinByteString $ C.pack

adaAssetClass :: AssetClass
adaAssetClass = assetClass Ada.adaSymbol Ada.adaToken