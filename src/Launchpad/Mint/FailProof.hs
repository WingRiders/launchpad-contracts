{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.FailProof where

import Launchpad.Types
import Plutarch
import Plutarch.Api.V2
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Mint.Util
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util
import PlutusLedgerApi.V2

pfailProofMintingPolicy :: Term s PMintingPolicy
pfailProofMintingPolicy = plam \_redeemer context ->
  popaque (perrorIfFalse #$ ptraceIfFalse "B0" $ pvalidateFailProofToken # context)

{- | The minting policy for the fail proof token.

Allows minting one token when the launchpad has failed to achieve the min commitment.

  - One fail proof token is minted
  - There is a single output with the minted token
  - The script address of that output is the minted token name
  - The datum of that output is a script hash
  - There is an input with the same script hash as the datum of the fail proof output
  - That input has a FailLaunchpad redeemer
-}
pvalidateFailProofToken :: Term s (PScriptContext :--> PBool)
pvalidateFailProofToken = plam \context -> unTermCont do
  contextF <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["inputs", "outputs", "mint", "redeemers"] contextF.txInfo
  failProofCs <- pletC (pownCurrencySymbol contextF.purpose)
  PPair failProofTn minted <- pmatchC (pvalueOfSingleton tx.mint failProofCs)
  let failProof =
        passertSingleSpecificInput "B1"
          # pid
          # ptokenNameAsScriptHash failProofTn
          # failProofCs
          # failProofTn
          # tx.outputs
  failProofF <- pletFieldsC @'["datum"] failProof
  PFailProofDatum nodeHash <-
    pmatchC (pfromPDatum @PFailProofDatum # (ptryFromInlineDatum # failProofF.datum))
  pure $
    pand'List
      [ ptraceIfFalse "B2" $ minted #== 1
      , ptraceIfFalse "B3" $
          pany
            # plam
              ( \i -> unTermCont do
                  iF <- pletFieldsC @'["resolved", "outRef"] i
                  oF <- pletFieldsC @'["address"] iF.resolved
                  pure $
                    (pscriptHashFromAddress # oF.address #== pjust (pfromData nodeHash))
                      #&& pisFailLaunchpadConstructor (ptryTxOutRefRedeemer # iF.outRef # tx.redeemers)
              )
            # pfromData tx.inputs
      ]

failProofPolicyScript :: Script
failProofPolicyScript = toScript pfailProofMintingPolicy

failProofPolicySymbol :: CurrencySymbol
failProofPolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash failProofPolicyScript
