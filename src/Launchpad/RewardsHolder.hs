{-# LANGUAGE BlockArguments #-}

module Launchpad.RewardsHolder where

import Launchpad.Types
import Plutarch
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext (pfromOutputDatum, ptxSignedBy)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Maybe
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Rewards Holder Validator

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data RewardsHolderConfig = RewardsHolderConfig
  { poolProofValidatorHash :: ScriptHash
  , poolProofSymbol :: CurrencySymbol
  }
  deriving stock (Generic)

PlutusTx.makeIsDataIndexed ''RewardsHolderConfig [('RewardsHolderConfig, 0)]
PlutusTx.makeLift ''RewardsHolderConfig

data PRewardsHolderConfig (s :: S)
  = PRewardsHolderConfig
      ( Term
          s
          ( PDataRecord
              [ "poolProofValidatorHash" ':= PScriptHash
              , "poolProofSymbol" ':= PCurrencySymbol
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRewardsHolderConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRewardsHolderConfig where
  type PLifted PRewardsHolderConfig = RewardsHolderConfig

deriving via
  (DerivePConstantViaData RewardsHolderConfig PRewardsHolderConfig)
  instance
    (PConstantDecl RewardsHolderConfig)

{- | This validates spending of a RewardsHolder eUTxO.
     In order to transaction be valid:
      1. The transaction has a PoolProof eUTxO with a PoolProof token in the reference inputs, the token name must be equal to the PoolProof validator hash
      2. The transaction is signed by the PubKeyHash stored in the first part of the node key from the rewards holder utxo datum
      3. The pool proof must have the same asset classes in the datum as the rewards utxo
-}
prewardsHolderValidator :: Term s (PRewardsHolderConfig :--> PRewardsHolderDatum :--> PScriptContext :--> PBool)
prewardsHolderValidator = phoistAcyclic $ plam \cfg datum context -> unTermCont do
  cfgF <- pletFieldsC @'["poolProofValidatorHash", "poolProofSymbol"] cfg
  contextFields <- pletFieldsC @'["txInfo"] context
  txInfoFields <- pletFieldsC @'["referenceInputs", "signatories", "datums"] contextFields.txInfo

  datumF <- pletFieldsC @'["owner", "projectSymbol", "projectToken", "raisingSymbol", "raisingToken"] datum

  let poolProofRefInput = ptryUniqueScriptTxInInfo cfgF.poolProofValidatorHash txInfoFields.referenceInputs
      signedByOwner = ptxSignedBy # txInfoFields.signatories # pdata (pcon (PPubKeyHash (pfromData (pfstBuiltin # (pfromData (datumF.owner))))))

  poolProofOut <- pletC (ptxInInfoResolved # poolProofRefInput)
  poolProofDatum <-
    pletFieldsC @'["projectSymbol", "projectToken", "raisingSymbol", "raisingToken"]
      (pfromJust # (pfromOutputDatum @PPoolProofDatum # (ptxOutDatum # poolProofOut) # txInfoFields.datums))

  pure $
    pand'List
      [ ptraceIfFalse "M1" (ptxOutHasAssociatedToken cfgF.poolProofSymbol poolProofOut)
      , ptraceIfFalse "M2" signedByOwner
      , ptraceIfFalse "M3" $
          pand'List
            [ ptraceIfFalse "M4" $ datumF.projectSymbol #== poolProofDatum.projectSymbol
            , ptraceIfFalse "M5" $ datumF.projectToken #== poolProofDatum.projectToken
            , ptraceIfFalse "M6" $ datumF.raisingSymbol #== poolProofDatum.raisingSymbol
            , ptraceIfFalse "M7" $ datumF.raisingToken #== poolProofDatum.raisingToken
            ]
      ]

rewardsHolderValidator :: Term s (PRewardsHolderConfig :--> PValidator)
rewardsHolderValidator = phoistAcyclic $ plam \cfg rawDatum _redeemer context ->
  let datum = ptryFrom @PRewardsHolderDatum rawDatum fst
   in popaque $ perrorIfFalse #$ ptraceIfFalse "M0" $ prewardsHolderValidator # cfg # datum # context

rewardsHolderScriptValidator :: RewardsHolderConfig -> Script
rewardsHolderScriptValidator cfg = toScript (rewardsHolderValidator # pconstant cfg)

rewardsHolderScriptValidatorHash :: RewardsHolderConfig -> ScriptHash
rewardsHolderScriptValidatorHash = scriptHash . rewardsHolderScriptValidator

rewardsHolderScriptAddress :: RewardsHolderConfig -> Address
rewardsHolderScriptAddress = scriptHashToAddress . rewardsHolderScriptValidatorHash

rewardsHolderScript :: Script
rewardsHolderScript = toScript rewardsHolderValidator
