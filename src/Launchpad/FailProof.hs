module Launchpad.FailProof where

import Plutarch
import Plutarch.Api.V2
import Plutarch.PlutusScript
import Plutus.Util
import PlutusLedgerApi.V2

failProofValidator :: Term s PValidator
failProofValidator = plam $ \_ _ _ -> perror

failProofScriptValidator :: Script
failProofScriptValidator = toScript failProofValidator

failProofScriptValidatorHash :: ScriptHash
failProofScriptValidatorHash = scriptHash failProofScriptValidator

failProofScriptAddress :: Address
failProofScriptAddress = scriptHashToAddress failProofScriptValidatorHash

failProofScript :: Script
failProofScript = toScript failProofValidator
