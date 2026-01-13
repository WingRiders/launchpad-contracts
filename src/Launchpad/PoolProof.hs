{-# LANGUAGE BlockArguments #-}

module Launchpad.PoolProof where

import Plutarch
import Plutarch.Api.V2
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutus.Util
import PlutusLedgerApi.V2

{- | Parameter of the Pool Proof Validator (CurrencySymbol of the Pool Proof Token)

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
type PoolProofConfig = CurrencySymbol

newtype PPoolProofConfig (s :: S) = PPoolProofConfig (Term s (PAsData PCurrencySymbol))
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PPoolProofConfig where
  type DPTStrat _ = PlutusTypeNewtype

-- | The pool proof can't be spent.
poolProofValidator :: Term s (PPoolProofConfig :--> PValidator)
poolProofValidator = plam \_cfg _datum _redeemer _context -> perror

poolProofScriptValidator :: PoolProofConfig -> Script
poolProofScriptValidator cfg = toScript (poolProofValidator # pcon (PPoolProofConfig (pdata (pconstant cfg))))

poolProofScriptValidatorHash :: PoolProofConfig -> ScriptHash
poolProofScriptValidatorHash = scriptHash . poolProofScriptValidator

poolProofScriptAddress :: PoolProofConfig -> Address
poolProofScriptAddress = scriptHashToAddress . poolProofScriptValidatorHash

poolProofScript :: Script
poolProofScript = toScript poolProofValidator
