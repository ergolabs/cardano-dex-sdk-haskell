module ErgoDex.Types where

import Prelude                            (Show, Eq, Integer, ($), (==))

import Ledger
import Ledger.Value                       (CurrencySymbol, TokenName, AssetClass(..), assetClassValueOf, assetClassValue, Value(..))
import PlutusTx.Numeric                   (AdditiveSemigroup(..), MultiplicativeSemigroup(..))
import qualified PlutusTx.AssocMap        as Map
import qualified Prelude                  as P

import ErgoDex.Contracts.Types

data Lovelace = Lovelace
  deriving (Show, Eq)

newtype AssetEntry = AssetEntry { unAssetEntry :: (AssetClass, Integer) }
  deriving (Show, Eq)

assetEntry :: CurrencySymbol -> TokenName -> Integer -> AssetEntry
assetEntry cs tn v = AssetEntry (AssetClass (cs, tn), v)

data AssetAmount a = AssetAmount
  { getAsset  :: Coin a
  , getAmount :: Amount a
  } deriving (Show, Eq)

instance AdditiveSemigroup (AssetAmount a) where
  a0 + a1 = a0 { getAmount = (getAmount a0) + (getAmount a1) }

instance MultiplicativeSemigroup (AssetAmount a) where
  a0 * a1 = a0 { getAmount = (getAmount a0) * (getAmount a1) }

retagCoin :: forall a b . Coin a -> Coin b
retagCoin (Coin ac) = Coin ac

assetAmountRawValue :: AssetAmount a -> Integer
assetAmountRawValue AssetAmount{getAmount=Amount v} = v

assetAmountValue :: AssetAmount a -> Value
assetAmountValue AssetAmount{getAsset=Coin ac, getAmount=Amount v} = assetClassValue ac v

assetAmountOf :: AssetEntry -> AssetAmount a
assetAmountOf (AssetEntry (ac, v)) = AssetAmount (Coin ac) (Amount v)

assetAmountCoinOf :: Coin a -> Integer -> AssetAmount a
assetAmountCoinOf c v = AssetAmount c (Amount v)

assetAmountPairOf :: (AssetEntry, AssetEntry) -> Coin a -> AssetAmount a
assetAmountPairOf (AssetEntry (ac, av), AssetEntry (bc, bv)) c =
  AssetAmount c (Amount $
    if ac == (unCoin c) then av
    else if bc == (unCoin c) then bv
    else 0)

assetAmountValueOf :: Value -> Coin a -> AssetAmount a
assetAmountValueOf v c =
  AssetAmount c (Amount $ assetClassValueOf v (unCoin c))

assetAmountSubstract :: Value -> AssetAmount a -> Value
assetAmountSubstract v AssetAmount{..} = let
	assetValue = Value $ Map.fromList [((P.fst $ unAssetClass (unCoin getAsset)), Map.fromList [((P.snd $ unAssetClass (unCoin getAsset)), (P.negate(unAmount getAmount)))])]
	in v P.<> assetValue

data ExFeePerToken = ExFeePerToken
  { exFeePerTokenNum :: Integer
  , exFeePerTokenDen :: Integer
  } deriving (Show, Eq)

newtype ExFee = ExFee { unExFee :: Amount Lovelace }
  deriving (Show, Eq)