{-# LANGUAGE BlockArguments #-}

module Launchpad.RewardsHolder where

import Launchpad.Constants
import Launchpad.Types
import Plutarch
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Types.Base
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
  , usesWr :: Bool
  , usesSundae :: Bool
  , endTime :: POSIXTime
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
              , "usesWr" ':= PBool
              , "usesSundae" ':= PBool
              , "endTime" ':= PPOSIXTime
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
      1. The transaction is signed by the PubKeyHash stored in the first part of the node key from the rewards holder utxo datum
      if usesWrV2 is true:
        2. The transaction has a PoolProof utxo with a PoolProof token in the reference inputs, the token name must be equal to the PoolProof validator hash
        3. The pool proof must have the same asset classes in the datum as the rewards utxo
        4. The pool proof has WR dex field
      if usesSundaeV3 is true:
        5. The transaction has a PoolProof utxo with a PoolProof token in the reference inputs, the token name must be equal to the PoolProof validator hash
        6. The pool proof must have the same asset classes in the datum as the rewards utxo
        7. The pool proof has Sundae dex field
      the validation requires AT LEAST one proof to be present, not both

     In case the pool creation is stalled for whatever reason, the rewards are unlocked once sufficient time has passed since the end of the withdrawal period.
-}
prewardsHolderValidator :: Term s PRewardsHolderConfig -> Term s PRewardsHolderDatum -> Term s PScriptContext -> Term s PBool
prewardsHolderValidator cfg datum context = unTermCont do
  cfgF <-
    pletFieldsC
      @'[ "poolProofValidatorHash"
        , "poolProofSymbol"
        , "usesWr"
        , "usesSundae"
        , "endTime"
        ]
      cfg
  contextFields <- pletFieldsC @'["txInfo"] context
  tx <- pletFieldsC @'["referenceInputs", "signatories", "datums", "validRange"] contextFields.txInfo
  datumF <-
    pletFieldsC
      @'[ "owner"
        , "projectSymbol"
        , "projectToken"
        , "raisingSymbol"
        , "raisingToken"
        ]
      datum

  PTimestamps lowerTime _ <- pmatchC (pfiniteTxValidityRangeTimestamps # tx.validRange)

  let hasCorrectPoolProof dex =
        pany
          # plam
            ( \o -> plet (ptxInInfoResolved # o) \poolProof ->
                (ppaysToCredential # cfgF.poolProofValidatorHash # poolProof)
                  #&& unTermCont do
                    poolProofDatum <-
                      pletFieldsC @'["projectSymbol", "projectToken", "raisingSymbol", "raisingToken", "dex"] $
                        pfromPDatum @PPoolProofDatum #$ ptryFromInlineDatum #$ ptxOutDatum # poolProof
                    pure $
                      pand'List
                        [ ptraceIfFalse "M1" (ptxOutHasAssociatedToken cfgF.poolProofSymbol poolProof)
                        , ptraceIfFalse "M2" $ datumF.projectSymbol #== poolProofDatum.projectSymbol
                        , ptraceIfFalse "M3" $ datumF.projectToken #== poolProofDatum.projectToken
                        , ptraceIfFalse "M4" $ datumF.raisingSymbol #== poolProofDatum.raisingSymbol
                        , ptraceIfFalse "M5" $ datumF.raisingToken #== poolProofDatum.raisingToken
                        , ptraceIfFalse "M6" $ poolProofDatum.dex #== dex
                        ]
            )
          # tx.referenceInputs

  let signedByOwner =
        ptxSignedBy
          # tx.signatories
          # pdata (pcon . PPubKeyHash . pfromData $ pfstBuiltin # pfromData datumF.owner)

  pure $
    pand'List
      [ ptraceIfFalse "M7" signedByOwner
      , (pto (lowerTime - cfgF.endTime) #> pconstant emergencyWithdrawalPeriod)
          #|| (pif cfgF.usesWr (ptraceIfFalse "M8" $ hasCorrectPoolProof (pcon PWr)) pfalse)
          #|| (pif cfgF.usesSundae (ptraceIfFalse "M9" $ hasCorrectPoolProof (pcon PSundae)) pfalse)
      ]

rewardsHolderValidator :: Term s (PRewardsHolderConfig :--> PValidator)
rewardsHolderValidator = plam \cfg rawDatum _redeemer context ->
  let datum = ptryFrom @PRewardsHolderDatum rawDatum fst
   in popaque $ perrorIfFalse #$ prewardsHolderValidator cfg datum context

rewardsHolderScriptValidator :: RewardsHolderConfig -> Script
rewardsHolderScriptValidator cfg = toScript (rewardsHolderValidator # pconstant cfg)

rewardsHolderScriptValidatorHash :: RewardsHolderConfig -> ScriptHash
rewardsHolderScriptValidatorHash = scriptHash . rewardsHolderScriptValidator

rewardsHolderScriptAddress :: RewardsHolderConfig -> Address
rewardsHolderScriptAddress = scriptHashToAddress . rewardsHolderScriptValidatorHash

rewardsHolderScript :: Script
rewardsHolderScript = toScript rewardsHolderValidator
