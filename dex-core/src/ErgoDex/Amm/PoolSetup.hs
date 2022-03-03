module ErgoDex.Amm.PoolSetup where

import           Control.Monad           (unless)
import           Data.Functor

import           Data.Either.Combinators (mapLeft)
import qualified Data.Set                as Set

import           Ledger          (PubKeyHash(..), Address, pubKeyHashAddress, PaymentPubKeyHash(..), StakePubKeyHash(..))
import qualified Ledger.Interval as Interval
import           Ledger.Value    (AssetClass)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Pool        (Pool(..), initPool)
import           ErgoDex.Amm.Constants
import qualified ErgoDex.Contracts.Typed as P
import           ErgoDex.Contracts.Types
import           CardanoTx.Models
import           ErgoDex.Contracts.Pool  (maxLqCapAmount)

data SetupExecError =
    MissingAsset AssetClass
  | MissingPool
  | InvalidNft
  | InvalidLiquidity
  | InsufficientInputs
  deriving Show

data PoolSetup = PoolSetup
  { poolDeploy :: PubKeyHash -> P.PoolConfig -> [FullTxOut] -> Either SetupExecError TxCandidate
  }

mkPoolSetup :: Address -> PoolSetup
mkPoolSetup changeAddr = PoolSetup
  { poolDeploy = poolDeploy' changeAddr
  }

poolDeploy' :: Address -> PubKeyHash -> P.PoolConfig -> [FullTxOut] -> Either SetupExecError TxCandidate
poolDeploy' changeAddr rewardPkh pp@P.PoolConfig{..} utxosIn = do
  inNft <- overallAmountOf utxosIn poolNft
  inLq  <- overallAmountOf utxosIn poolLq
  inX   <- overallAmountOf utxosIn poolX
  inY   <- overallAmountOf utxosIn poolY

  unless (amountEq inNft 1) (Left InvalidNft) -- make sure valid NFT is provided
  unless (getAmount inLq == maxLqCapAmount) (Left InvalidLiquidity) -- make sure valid amount of LQ tokens is provided

  (Predicted poolOutput nextPool, unlockedLq) <-
    mapLeft (const InvalidLiquidity) (initPool pp (getAmount inX, getAmount inY))

  let
    mintLqValue  = coinAmountValue (poolCoinLq nextPool) unlockedLq
    rewardOutput = TxOutCandidate
      { txOutCandidateAddress = pubKeyHashAddress (PaymentPubKeyHash rewardPkh) (Nothing :: Maybe StakePubKeyHash) -- todo: stake addr
      , txOutCandidateValue   = mintLqValue <> minSafeOutputValue
      , txOutCandidateDatum   = Nothing
      }

    inputs  = utxosIn <&> mkPkhTxIn
    outputs = [poolOutput, rewardOutput]

    overallAdaOut = assetAmountOfCoin totalValueIn adaCoin
      where totalValueIn = foldr ( (<>) . txOutCandidateValue) mempty outputs

  overallAdaIn <- overallAmountOf utxosIn adaCoin
  unless (overallAdaIn >= overallAdaOut) (Left InsufficientInputs)

  Right $ TxCandidate
    { txCandidateInputs       = Set.fromList inputs
    , txCandidateOutputs      = [poolOutput, rewardOutput]
    , txCandidateValueMint    = mempty -- todo: mint NFT and LQ right there?
    , txCandidateMintInputs   = mempty
    , txCandidateChangePolicy = Just $ ReturnTo changeAddr
    , txCandidateValidRange   = Interval.always
    }

overallAmountOf :: [FullTxOut] -> Coin a -> Either SetupExecError (AssetAmount a)
overallAmountOf utxos c =
    if amountEq coinAmount 0
    then Left $ MissingAsset $ unCoin c
    else Right coinAmount
  where
    totalValueIn = foldr ( (<>) . fullTxOutValue) mempty utxos
    coinAmount   = assetAmountOfCoin totalValueIn c
