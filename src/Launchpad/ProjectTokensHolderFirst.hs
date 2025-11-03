{-# LANGUAGE BlockArguments #-}

module Launchpad.ProjectTokensHolderFirst where

import Launchpad.Constants
import Launchpad.Types
import Plutarch
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
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

{- | Parameters of the Project Tokens Holder First Validator

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.

     Note: daoAdmin should not be equal to commitFoldOwner, otherwise possible compensation double-satisfaction is introduced
-}
data TokensHolderFirstConfig = TokensHolderFirstConfig
  { owner :: Address
  , startTime :: POSIXTime
  , projectTokensHolderSymbol :: CurrencySymbol
  , starter :: TxOutRef
  , withdrawalEndTime :: POSIXTime
  , daoAdmin :: PubKeyHash
  , usesWr :: Bool
  , usesSundae :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''TokensHolderFirstConfig [('TokensHolderFirstConfig, 0)]
PlutusTx.makeLift ''TokensHolderFirstConfig

data PTokensHolderFirstConfig (s :: S)
  = PTokensHolderFirstConfig
      ( Term
          s
          ( PDataRecord
              [ "owner" ':= PAddress
              , "startTime" ':= PPOSIXTime
              , "projectTokensHolderSymbol" ':= PCurrencySymbol
              , "starter" ':= PTxOutRef
              , "withdrawalEndTime" ':= PPOSIXTime
              , "daoAdmin" ':= PPubKeyHash
              , -- REVIEW: do we ensure at least one is true?
                "usesWr" ':= PBool
              , "usesSundae" ':= PBool
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PTokensHolderFirstConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTokensHolderFirstConfig where
  type PLifted PTokensHolderFirstConfig = TokensHolderFirstConfig

deriving via
  (DerivePConstantViaData TokensHolderFirstConfig PTokensHolderFirstConfig)
  instance
    (PConstantDecl TokensHolderFirstConfig)

projectTokensHolderValidator :: Term s (PTokensHolderFirstConfig :--> PValidator)
projectTokensHolderValidator =
  plam \cfg datum redeemer context ->
    let scenario = ptryFrom @PLaunchpadTokensHolderFirstRedeemer redeemer fst
        tokensDatum = ptryFrom @PLaunchpadTokensHolderDatum datum fst
     in popaque $ perrorIfFalse #$ ptraceIfFalse "K0" $ projectTokensHolderFirstValidator cfg tokensDatum scenario context

pvalidateLaunchpadCancellation ::
  Term s PAddress ->
  Term s PPOSIXTime ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PScriptHash ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PValue anyKey anyAmount) ->
  Term s PPOSIXTimeRange ->
  Term s PBool
pvalidateLaunchpadCancellation owner startTime holderCs validityTokenCount selfValidatorHash signatories mint timeInterval =
  pmatch (pfiniteTxValidityRangeTimestamps # timeInterval) \(PTimestamps _ upper) ->
    pand'List
      [ ptraceIfFalse "K1" $ ptxSignedBy # signatories # (paddressPubKeyCredential # owner)
      , ptraceIfFalse "K2" $ upper #< startTime
      , ptraceIfFalse "K3" $ pvalueOf # mint # holderCs # pscriptHashToTokenName selfValidatorHash #== -validityTokenCount
      ]

pnodeWithRewardsOrFailurePresent ::
  Term s PScriptHash ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PMap 'Unsorted PScriptPurpose PRedeemer) ->
  Term s PBool
pnodeWithRewardsOrFailurePresent nodeScriptHash inputs redeemers = plet nodeScriptHash \node ->
  pany
    # plam
      ( \i ->
          (ppaysToCredential # node # (ptxInInfoResolved # i))
            #&& plet
              (ptryFrom @PNodeRedeemer (pto (ptryTxOutRefRedeemer # (pfield @"outRef" # i) # redeemers)) fst)
              ( (flip pmatch)
                  \case
                    PDelegateToRewardsFold _ -> pconstant True
                    PFailLaunchpad _ -> pconstant True
                    _ -> pconstant False
              )
      )
    # inputs

{- | The first project tokens holder script validation supports launchpad cancellation and rewards folding.

Rewards fold:
  - There is a utxo in the inputs with a list element token
    with its token name equal to the nodeScriptHash field,
    the redeemer of the list element utxo must be a rewards fold
  - The launchpad holder utxo has one launchpad holder token
    with its token name equal to the script hash of the launchpad holder validator

Launchpad cancellation:
  - One launchpad holder token is burned
  - The upper current time approximation < start time
  - The transaction is signed by the owner of the launchpad
  - The launchpad holder utxo has one launchpad holder token
    with its token name equal to the script hash of the launchpad holder validator

Emergency withdrawal:
  - 1 project tokens holder first utxo is consumed, its validator is run
  - there is only one script utxo in the inputs
  - 1 project tokens holder token is burned with its token name equal to the script hash of the project tokens holder validator
  - the only currency symbol being burned is the project tokens holder symbol (equal to 2, because mint always contains 0 ADA)
  - at least "emergencyWithdrawalPeriod" amount of time has passed since the withdrawalEndTime
  - transaction is signed by the dao wallet
-}
projectTokensHolderFirstValidator ::
  Term s PTokensHolderFirstConfig ->
  Term s PLaunchpadTokensHolderDatum ->
  Term s PLaunchpadTokensHolderFirstRedeemer ->
  Term s PScriptContext ->
  Term s PBool
projectTokensHolderFirstValidator cfg datum redeemer context = unTermCont do
  cfgF <- pletFieldsC @'["owner", "startTime", "projectTokensHolderSymbol", "withdrawalEndTime", "daoAdmin", "usesWr", "usesSundae"] cfg
  holderCs <- pletC cfgF.projectTokensHolderSymbol
  contextFields <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["inputs", "referenceInputs", "redeemers", "signatories", "mint", "validRange"] contextFields.txInfo
  ownInput <- pletC (pownInput # contextFields.purpose # tx.inputs)
  selfValidatorHash <- pletC (pgetValidatorHashFromScriptAddress #$ ptxOutAddress # ownInput)
  let nodeScriptHash = pfromData (pto datum)
  PTimestamps lower _ <- pmatchC (pfiniteTxValidityRangeTimestamps # tx.validRange)

  expectedValidityCount <-
    pletC $
      pcond
        [ -- Two tokens when two dexes are used
          (cfgF.usesWr #&& cfgF.usesSundae, 2)
        , -- One token when only one dex is used
          (cfgF.usesWr #|| cfgF.usesSundae, 1)
        ]
        (ptraceError "K9")

  pure $
    pand'List
      [ ptxOutHasAssociatedTokens expectedValidityCount holderCs ownInput
      , pmatch
          redeemer
          \case
            PCancelLaunchpad _ ->
              pvalidateLaunchpadCancellation
                cfgF.owner
                cfgF.startTime
                holderCs
                expectedValidityCount
                selfValidatorHash
                tx.signatories
                tx.mint
                tx.validRange
            -- Covers both the rewards fold application and the failure case when not enough commitments were made
            PDelegateToRewardsOrFailure _ -> pnodeWithRewardsOrFailurePresent nodeScriptHash tx.inputs tx.redeemers
            PFirstTokensHolderEmergencyWithdrawal _ -> pfirstHolderEmergencyWithdrawal selfValidatorHash holderCs expectedValidityCount lower cfgF.daoAdmin tx.inputs tx.mint cfgF.withdrawalEndTime tx.signatories
      ]

pfirstHolderEmergencyWithdrawal ::
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PPOSIXTime ->
  Term s PPubKeyHash ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PPOSIXTime ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PBool
pfirstHolderEmergencyWithdrawal selfValidatorHash holderSymbol validityTokenCount lowerTime daoAdmin inputs mint withdrawalEndTime signatories = unTermCont do
  pure $
    pand'List
      [ ptraceIfFalse "K4" $ pcountAllScriptInputs # inputs #== 1
      , ptraceIfFalse "K5" $ pvalueOf # mint # holderSymbol # pscriptHashToTokenName selfValidatorHash #== -validityTokenCount
      , ptraceIfFalse "K6" $ plength # ((pto . pto) mint) #== 2
      , ptraceIfFalse "K7" $ pto (lowerTime - withdrawalEndTime) #> pconstant emergencyWithdrawalPeriod
      , ptraceIfFalse "K8" $ ptxSignedByPkh # pdata daoAdmin # signatories
      ]

projectTokensHolderScriptValidator :: TokensHolderFirstConfig -> Script
projectTokensHolderScriptValidator cfg = toScript (projectTokensHolderValidator # pconstant cfg)

projectTokensHolderScriptValidatorHash :: TokensHolderFirstConfig -> ScriptHash
projectTokensHolderScriptValidatorHash = scriptHash . projectTokensHolderScriptValidator

projectTokensHolderScriptAddress :: TokensHolderFirstConfig -> Address
projectTokensHolderScriptAddress = scriptHashToAddress . projectTokensHolderScriptValidatorHash

projectTokensHolderScript :: Script
projectTokensHolderScript = toScript projectTokensHolderValidator
