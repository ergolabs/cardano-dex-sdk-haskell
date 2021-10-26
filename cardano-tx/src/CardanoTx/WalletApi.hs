module CardanoTx.WalletApi where

import Plutus.V1.Ledger.Tx as P
import Ledger

data Constr
  = ValueAtLeast Value
  | ValueAtMost Value

data WalletApi f = WalletApi
  { selectUtxos :: Maybe Constr -> f [P.TxOut]
  , submitTx    :: P.Tx -> f P.TxId
  }
