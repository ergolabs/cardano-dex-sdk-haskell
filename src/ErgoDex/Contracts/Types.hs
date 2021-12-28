{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Types where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, ToJSON)
import Schema       (ToSchema)

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClassValue, assetClassValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)

data Lovelace = Lovelace deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''Lovelace [('Lovelace, 0)]
PlutusTx.makeLift ''Lovelace

-- Pool NFT
data Nft = Nft deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''Nft [('Nft, 0)]
PlutusTx.makeLift ''Nft

-- First asset of a pool
data X = X deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''X [('X, 0)]
PlutusTx.makeLift ''X

-- Second asset of a pool
data Y = Y deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''Y [('Y, 0)]
PlutusTx.makeLift ''Y

-- Liquidity token of a pool
data Liquidity = Liquidity deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''Liquidity [('Liquidity, 0)]
PlutusTx.makeLift ''Liquidity

-- First asset of a pool
data Quote = Quote deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''Quote [('Quote, 0)]
PlutusTx.makeLift ''Quote

-- Second asset of a pool
data Base = Base deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''Base [('Base, 0)]
PlutusTx.makeLift ''Base

-- Type to distinguish tokens within a pool
newtype Coin a = Coin { unCoin :: AssetClass }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Haskell.Eq, Haskell.Ord)
PlutusTx.makeIsDataIndexed ''Coin [('Coin, 0)]
PlutusTx.makeLift ''Coin

{-# INLINABLE retagCoin #-}
retagCoin :: forall a b . Coin a -> Coin b
retagCoin (Coin ac) = Coin ac

{-# INLINABLE valueOf #-}
valueOf :: Value -> Coin a -> Integer
valueOf v = assetClassValueOf v . unCoin

-- Difference of a token amount
newtype Diff a = Diff { unDiff :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
PlutusTx.makeIsDataIndexed ''Diff [('Diff, 0)]
PlutusTx.makeLift ''Diff

-- Amount of a token
newtype Amount a = Amount { unAmount :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
PlutusTx.makeIsDataIndexed ''Amount [('Amount, 0)]
PlutusTx.makeLift ''Amount

{-# INLINABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINABLE isUnit #-}
isUnit :: Value -> Coin a -> Bool
isUnit v c = amountOf v c == 1

{-# INLINABLE coinAmountValue #-}
coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v
