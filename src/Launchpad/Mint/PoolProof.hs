{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.PoolProof where

import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Launchpad.Types
import Launchpad.Util
import Plutarch
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.Field (pletAllC)
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
  { poolValidatorHash :: ScriptHash
  , poolSymbol :: CurrencySymbol
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''PoolProofPolicyConfig [('PoolProofPolicyConfig, 0)]
PlutusTx.makeLift ''PoolProofPolicyConfig

data PPoolProofPolicyConfig (s :: S)
  = PPoolProofPolicyConfig
      ( Term
          s
          ( PDataRecord
              [ "poolValidatorHash" ':= PScriptHash
              , "poolSymbol" ':= PCurrencySymbol
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
      1. There is a pool eUTxO with the pool validity token in the reference inputs.
      2. The assets pair of the pool is a committed tokens/project tokens pair stored in the inline datum.
      3. 1 pool proof token is minted, its token name is equal to the validator hash of the utxo where it's placed.
      4. There is only ADA and pool proof token in the pool proof output.
-}
pvalidatePoolProofMinting :: Term s (PPoolProofPolicyConfig :--> PScriptContext :--> PBool)
pvalidatePoolProofMinting = phoistAcyclic $ plam \cfg context -> unTermCont do
  cfgF <- pletFieldsC @'["poolValidatorHash", "poolSymbol"] cfg
  contextFields <- pletFieldsC @'["txInfo", "purpose"] context
  txInfoFields <- pletFieldsC @'["mint", "inputs", "outputs", "referenceInputs", "datums"] contextFields.txInfo
  poolProofCs <- pletC (pownCurrencySymbol contextFields.purpose)
  PPair poolProofTn poolProofMinted <- pmatchC (pvalueOfSingleton txInfoFields.mint poolProofCs)
  poolProofOut <-
    pletFieldsC @'["datum", "value"]
      ( passertSingleSpecificInput "D1"
          # pid
          # ptokenNameAsScriptHash poolProofTn
          # poolProofCs
          # poolProofTn
          # txInfoFields.outputs
      )

  let poolRefInput =
        passertSingleSpecificInput "D2"
          # ptxInInfoResolved
          # cfgF.poolValidatorHash
          # cfgF.poolSymbol
          # (pconstant C.lpValidityTokenName)
          # txInfoFields.referenceInputs

  poolTxIn <- pletFieldsC @'["datum", "value"] poolRefInput
  let poolDatum = ptryFromInlineDatum # poolTxIn.datum
  poolDatumF <- pletFieldsC @'["assetASymbol", "assetBSymbol", "assetAToken", "assetBToken"] (pfromPDatum @PPoolConstantProductDatum # poolDatum)

  datumF <- pletAllC (pfromPDatum @PPoolProofDatum # (ptryFromInlineDatum # poolProofOut.datum))

  let correctPool =
        pisCorrectPool
          (pfromData datumF.projectSymbol, pfromData datumF.projectToken)
          (pfromData datumF.raisingSymbol, pfromData datumF.raisingToken)
          (poolDatumF.assetASymbol, poolDatumF.assetAToken)
          (poolDatumF.assetBSymbol, poolDatumF.assetBToken)

  pure $
    pand'List
      [ ptraceIfFalse "D3" (poolProofMinted #== 1)
      , ptraceIfFalse "D4" (pcountOfUniqueTokens # poolProofOut.value #== 2)
      , ptraceIfFalse "D5" correctPool
      , -- 0 is WingRidersV2
        -- 1 is SundaeSwapV3
        ptraceIfFalse "D6" (pfromData datumF.dex #== 0)
      ]

ppoolProofMintingPolicy :: Term s (PPoolProofPolicyConfig :--> PMintingPolicy)
ppoolProofMintingPolicy = phoistAcyclic $ plam \cfg _redeemer context ->
  popaque $ perrorIfFalse #$ ptraceIfFalse "D0" $ pvalidatePoolProofMinting # cfg # context

poolProofMintingPolicy :: PoolProofPolicyConfig -> Script
poolProofMintingPolicy cfg = toScript $ ppoolProofMintingPolicy # pconstant cfg

poolProofMintingPolicySymbol :: PoolProofPolicyConfig -> CurrencySymbol
poolProofMintingPolicySymbol cfg = CurrencySymbol $ getScriptHash $ scriptHash (poolProofMintingPolicy cfg)

poolProofPolicyScript :: Script
poolProofPolicyScript = toScript ppoolProofMintingPolicy

poolProofPolicySymbol :: CurrencySymbol
poolProofPolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash poolProofPolicyScript
