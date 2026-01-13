module Other.FreePolicy where

import Plutarch
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  scriptHash,
 )
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  ScriptHash (..),
 )

pvalidateFree :: Term s (PData :--> PScriptContext :--> PUnit)
pvalidateFree = plam $ \_ _ -> pconstant ()

pvalidateFreePolicy :: Term s PMintingPolicy
pvalidateFreePolicy = plam $ \redeemer context -> popaque $ pvalidateFree # redeemer # context

freeMintingPolicy :: Script
freeMintingPolicy = toScript pvalidateFreePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = CurrencySymbol $ getScriptHash $ scriptHash freeMintingPolicy
