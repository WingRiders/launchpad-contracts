{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.PoolProof where

import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Launchpad.Types
import Launchpad.Util
import Plutarch
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Mint.Util
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Pool Proof Minting Policy

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data PoolProofPolicyConfig = PoolProofPolicyConfig
  { wrPoolValidatorHash :: ScriptHash
  , wrPoolSymbol :: CurrencySymbol
  , sundaePoolScriptHash :: ScriptHash
  -- ^ Both validator and minting policy share the script
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''PoolProofPolicyConfig [('PoolProofPolicyConfig, 0)]
PlutusTx.makeLift ''PoolProofPolicyConfig

data PPoolProofPolicyConfig (s :: S)
  = PPoolProofPolicyConfig
      ( Term
          s
          ( PDataRecord
              [ "wrPoolValidatorHash" ':= PScriptHash
              , "wrPoolSymbol" ':= PCurrencySymbol
              , "sundaePoolScriptHash" ':= PScriptHash
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PPoolProofPolicyConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PPoolProofPolicyConfig where
  type PLifted PPoolProofPolicyConfig = PoolProofPolicyConfig

deriving via
  (DerivePConstantViaData PoolProofPolicyConfig PPoolProofPolicyConfig)
  instance
    (PConstantDecl PoolProofPolicyConfig)

{- | This validates minting of a PoolProof token and creation of PoolProof.
     In order to transaction be valid:
      1. There is a WR V2 pool utxo with the pool validity token in the reference inputs if passed redeemer is Wr.
      2. There is a Sundae V3 pool utxo with the pool validity token in the reference inputs if passed redeemer is Sundae.
      3. The passed redeemer is equal to the dex field of the pool proof output datum.
      4. The assets pair of the pool is a committed tokens/project tokens pair stored in the inline datum.
      5. 1 pool proof token is minted, its token name is equal to the validator hash of the utxo where it's placed.
      6. There is only ADA and pool proof token in the pool proof output.
-}
pvalidatePoolProofMinting :: Term s PPoolProofPolicyConfig -> Term s PDex -> Term s PScriptContext -> Term s PBool
pvalidatePoolProofMinting cfg dex context = unTermCont do
  cfgF <- pletFieldsC @'["wrPoolValidatorHash", "wrPoolSymbol", "sundaePoolScriptHash"] cfg
  contextFields <- pletFieldsC @'["txInfo", "purpose"] context
  tx <- pletFieldsC @'["referenceInputs", "mint", "outputs", "inputs"] contextFields.txInfo
  poolProofCs <- pletC (pownCurrencySymbol contextFields.purpose)
  PPair poolProofTn poolProofMinted <- pmatchC (pvalueOfSingleton tx.mint poolProofCs)
  poolProofOut <-
    pletFieldsC @'["datum", "value"]
      ( passertSingleSpecificInput "D1"
          # pid
          # ptokenNameAsScriptHash poolProofTn
          # poolProofCs
          # poolProofTn
          # tx.outputs
      )

  datumF <-
    pletFieldsC @'["projectSymbol", "projectToken", "raisingSymbol", "raisingToken", "dex"]
      (pfromPDatum @PPoolProofDatum # (ptryFromInlineDatum # poolProofOut.datum))

  let isCorrectPool =
        pmatch dex \case
          PWr -> unTermCont do
            let poolRefInput =
                  passertSingleSpecificInput "D2"
                    # ptxInInfoResolved
                    # cfgF.wrPoolValidatorHash
                    # cfgF.wrPoolSymbol
                    # (pconstant C.wrLpValidityTokenName)
                    # tx.referenceInputs
            pool <- pletFieldsC @'["datum"] poolRefInput
            poolDatum <-
              pletFieldsC @'["assetASymbol", "assetAToken", "assetBSymbol", "assetBToken"]
                ( pfromPDatum @PWrPoolConstantProductDatum
                    #$ ptryFromInlineDatum
                    # pool.datum
                )
            pure $
              pisCorrectPool
                (pfromData datumF.projectSymbol, pfromData datumF.projectToken)
                (pfromData datumF.raisingSymbol, pfromData datumF.raisingToken)
                (poolDatum.assetASymbol, poolDatum.assetAToken)
                (poolDatum.assetBSymbol, poolDatum.assetBToken)
          PSundae -> unTermCont do
            let poolRefInput =
                  ptxInInfoResolved
                    #$ passertSingleton "D3"
                    #$ pfilter
                    # plam (\i -> ppaysToCredential # cfgF.sundaePoolScriptHash #$ ptxInInfoResolved # i)
                    # tx.referenceInputs
            pool <- pletFieldsC @'["datum", "value"] poolRefInput

            poolDatum <-
              pletFieldsC @'["assets", "identifier"]
                ( pfromPDatum @PSundaePoolDatum
                    #$ ptryFromInlineDatum
                    # pool.datum
                )

            assets <- pletFieldsC @'["a", "b"] poolDatum.assets
            assetA <- pletFieldsC @'["currencySymbol", "tokenName"] $ assets.a
            assetB <- pletFieldsC @'["currencySymbol", "tokenName"] $ assets.b

            let assetASymbol = pfromData assetA.currencySymbol
            let assetAToken = pfromData assetA.tokenName

            let assetBSymbol = pfromData assetB.currencySymbol
            let assetBToken = pfromData assetB.tokenName

            pure $
              pand'List
                [ pisCorrectPool
                    (pfromData datumF.projectSymbol, pfromData datumF.projectToken)
                    (pfromData datumF.raisingSymbol, pfromData datumF.raisingToken)
                    (assetASymbol, assetAToken)
                    (assetBSymbol, assetBToken)
                , pvalueOf
                    # pool.value
                    # (pcon . PCurrencySymbol . pto . pfromData $ cfgF.sundaePoolScriptHash)
                    # poolSundaeNftName poolDatum.identifier
                    #== 1
                ]

  pure $
    pand'List
      [ ptraceIfFalse "D4" (poolProofMinted #== 1)
      , ptraceIfFalse "D5" (pcountOfUniqueTokens # poolProofOut.value #== 2)
      , ptraceIfFalse "D6" isCorrectPool
      , ptraceIfFalse "D7" (datumF.dex #== dex)
      ]

ppoolProofMintingPolicy :: Term s (PPoolProofPolicyConfig :--> PMintingPolicy)
ppoolProofMintingPolicy = plam \cfg redeemer context ->
  popaque $
    perrorIfFalse
      #$ pvalidatePoolProofMinting cfg (pfromData (ptryFrom @(PAsData PDex) redeemer fst)) context

poolProofMintingPolicy :: PoolProofPolicyConfig -> Script
poolProofMintingPolicy cfg = toScript $ ppoolProofMintingPolicy # pconstant cfg

poolProofMintingPolicySymbol :: PoolProofPolicyConfig -> CurrencySymbol
poolProofMintingPolicySymbol cfg = CurrencySymbol $ getScriptHash $ scriptHash (poolProofMintingPolicy cfg)

poolProofPolicyScript :: Script
poolProofPolicyScript = toScript ppoolProofMintingPolicy

poolProofPolicySymbol :: CurrencySymbol
poolProofPolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash poolProofPolicyScript
