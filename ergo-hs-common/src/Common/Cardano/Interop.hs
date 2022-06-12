module Common.Cardano.Interop where

import qualified Cardano.Api          as C
import qualified Plutus.V1.Ledger.Api as P
import qualified PlutusTx.Prelude     as PlutusTx

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> P.PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = P.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash
