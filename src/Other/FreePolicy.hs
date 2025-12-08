module Other.FreePolicy where

import Plutarch
import Plutarch.Api.V2 (
  PMintingPolicy,
  scriptHash,
 )
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  ScriptHash (..),
 )

pvalidateFreePolicy :: Term s PMintingPolicy
pvalidateFreePolicy = plam (\_redeemer _context -> popaque (pconstant ()))

freeMintingPolicy :: Script
freeMintingPolicy = toScript pvalidateFreePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = CurrencySymbol . getScriptHash . scriptHash $ freeMintingPolicy
