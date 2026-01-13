{-# LANGUAGE BlockArguments #-}

module Other.Mint.FreeNft (pfreeNftMintingPolicy, freeNftPolicyScript) where

import Plutarch
import Plutarch.Api.V2
import Plutarch.Extra.TermCont
import Plutarch.Mint.Util
import Plutarch.Pair
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util

{- | Allows any user to mint one token at a time
The token name is equal to the out ref id of the first input, that (almost) ensures the uniqueness of the token
-}
pfreeNftMintingPolicy :: Term s (a :--> PMintingPolicy)
pfreeNftMintingPolicy = plam \_cfg _redeemer context ->
  popaque $
    perrorIfFalse #$ unTermCont do
      ctx <- pletFieldsC @'["txInfo", "purpose"] context
      tx <- pletFieldsC @'["mint", "inputs"] ctx.txInfo
      let firstInputId = pfield @"_0" #$ pfield @"id" #$ pfield @"outRef" #$ phead # pfromData tx.inputs
          freetNftSymbol = pownCurrencySymbol ctx.purpose
      PPair freetNftToken minted <- pmatchC (pvalueOfSingleton tx.mint freetNftSymbol)

      pure $
        pand'List
          [ minted #== 1
          , firstInputId #== pto freetNftToken
          ]

freeNftPolicyScript :: Script
freeNftPolicyScript = toScript pfreeNftMintingPolicy
