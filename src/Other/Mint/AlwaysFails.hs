module Other.Mint.AlwaysFails where

import Plutarch
import Plutarch.Api.V2 (
  PMintingPolicy,
  scriptHash,
 )
import Plutarch.PlutusScript (toScript)
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  ScriptHash (..),
 )

palwaysFailsMintingPolicy :: Term s PMintingPolicy
palwaysFailsMintingPolicy = plam $ \_ _ -> perror

alwaysFailsMintingPolicy :: Script
alwaysFailsMintingPolicy = toScript palwaysFailsMintingPolicy

alwaysFailsCurrencySymbol :: CurrencySymbol
alwaysFailsCurrencySymbol = CurrencySymbol $ getScriptHash $ scriptHash alwaysFailsMintingPolicy
