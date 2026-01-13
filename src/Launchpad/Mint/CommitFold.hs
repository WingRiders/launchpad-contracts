{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.CommitFold where

import Launchpad.Constants (expectedPkhLength, maxNodeIndex, minNodeIndex)
import Launchpad.Types
import Plutarch
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Mint.Util
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Commit Fold Minting Policy

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data CommitFoldPolicyConfig = CommitFoldPolicyConfig
  { starter :: TxOutRef
  , contributionEndTime :: POSIXTime
  , withdrawalEndTime :: POSIXTime
  , nodeSymbol :: CurrencySymbol
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''CommitFoldPolicyConfig [('CommitFoldPolicyConfig, 0)]
PlutusTx.makeLift ''CommitFoldPolicyConfig

data PCommitFoldPolicyConfig (s :: S)
  = PCommitFoldPolicyConfig
      ( Term
          s
          ( PDataRecord
              [ "starter" ':= PTxOutRef
              , "contributionEndTime" ':= PPOSIXTime
              , "withdrawalEndTime" ':= PPOSIXTime
              , "nodeSymbol" ':= PCurrencySymbol
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PCommitFoldPolicyConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PCommitFoldPolicyConfig where
  type PLifted PCommitFoldPolicyConfig = CommitFoldPolicyConfig

deriving via
  (DerivePConstantViaData CommitFoldPolicyConfig PCommitFoldPolicyConfig)
  instance
    (PConstantDecl CommitFoldPolicyConfig)

{- | Commit fold minting policy.

Supports initializing the fold and unconditionally burning the token.

Initialize the fold:
  - 1 commit fold token is minted, its token name is equal to the validator hash of the K utxo where it's placed
  - there should be a reference input with the validator hash equal to the commit fold nodeScriptHash field
    and one node token with its token name equal to the nodeScriptHash
  - the key field of the node is nothing
  - the next field of K is equal to the next field of the node
  - the nodeCount field of K is 1
  - the lower approximation of the current time is greater than the withdrawal end time of the launchpad
  - the owner field of K is a pubkey address
  - the owner address staking credential is not StakingPtr
  - the committed field of K is 0
  - if cutoffTime field of K is Just
    - the overcommitted field of K is not negative
    - cutoffTime field of K is lower than contributionEndTime
    - cutoffKey field of K is Just
    - cutoffKey bytestring is 28 bytes (1B bytestrings do not need to be supported, since there will always be a 28B option)
    - cutoffKey index is [0, 1000]
  - if cutoffTime field of K is Nothing
    - overcommitted field of K is 0
    - cutoffKey field of K is Nothing

Burn:
  - one commit fold validity token is burned
-}
pcommitFoldMintingPolicy :: Term s (PCommitFoldPolicyConfig :--> PMintingPolicy)
pcommitFoldMintingPolicy = plam \cfg _redeemer context ->
  popaque (perrorIfFalse #$ ptraceIfFalse "A0" $ pvalidateCommitFoldToken # cfg # context)

pvalidateCommitFoldToken :: Term s (PCommitFoldPolicyConfig :--> PScriptContext :--> PBool)
pvalidateCommitFoldToken = plam \cfg context -> unTermCont do
  cfgF <- pletFieldsC @'["contributionEndTime", "withdrawalEndTime", "nodeSymbol"] cfg
  contextFields <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["outputs", "mint", "referenceInputs", "validRange"] contextFields.txInfo
  foldCs <- pletC (pownCurrencySymbol contextFields.purpose)
  PPair foldTn mintedAmount <- pmatchC (pvalueOfSingleton tx.mint foldCs)
  nodeCs <- pletC cfgF.nodeSymbol
  let timestamps = pfiniteTxValidityRangeTimestamps # tx.validRange
  pure $
    pcond
      [
        ( mintedAmount #== 1
        , pvalidateCommitTokenInitialization
            cfgF.contributionEndTime
            cfgF.withdrawalEndTime
            foldCs
            foldTn
            nodeCs
            tx.outputs
            tx.referenceInputs
            timestamps
        )
      , (mintedAmount #== (-1), ptrue)
      ]
      (ptraceError "minted wrong commit fold tokens amount")

pvalidateCommitTokenInitialization ::
  Term s PPOSIXTime ->
  Term s PPOSIXTime ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s PTimestamps ->
  Term s PBool
pvalidateCommitTokenInitialization contributionEndTime withdrawalEndTime foldSymbol foldToken nodeSymbol outputs referenceInputs timestamps = unTermCont do
  PTimestamps currentTime _ <- pmatchC timestamps
  foldF <-
    pletFieldsC @'["datum", "value"]
      ( passertSingleSpecificInput "one commit fold output allowed"
          # plam id
          # ptokenNameAsScriptHash foldToken
          # foldSymbol
          # foldToken
          # outputs
      )
  foldD' <- pletC $ pfromPDatum @PCommitFoldDatum # (ptryFromInlineDatum # foldF.datum)
  foldD <- pletFieldsC @'["nodeScriptHash", "next", "committed", "cutoffKey", "cutoffTime", "overcommitted", "nodeCount", "owner"] foldD'

  nodeScriptHash <- pletC foldD.nodeScriptHash
  nodeF <-
    pletFieldsC @'["datum", "value"]
      ( passertSingleSpecificInput "one node reference input allowed"
          # ptxInInfoResolved
          # nodeScriptHash
          # nodeSymbol
          # pscriptHashToTokenName nodeScriptHash
          # referenceInputs
      )
  nodeD <- pletFieldsC @'["key", "next"] $ pfromPDatum @PNode # (ptryFromInlineDatum # nodeF.datum)
  pure $
    pand'List
      [ ptraceIfFalse "A1" $ currentTime #> withdrawalEndTime
      , ptraceIfFalse "A2" $ pfromData foldD.committed #== 0
      , ptraceIfFalse "A3" $ foldD.next #== nodeD.next
      , ptraceIfFalse "A4" $ pfromData foldD.nodeCount #== 1
      , ptraceIfFalse "A5" $ pisPubKeyAddress foldD.owner
      , ptraceIfFalse "A6" $ pnot # (pisStakePtrAddress foldD.owner)
      , pmatch foldD.cutoffTime \case
          PDJust timeR ->
            plet (pfromData . pfromData $ pfield @"_0" # timeR) \time ->
              plet (pfromData $ pfromDJust # foldD.cutoffKey) \key ->
                pand'List
                  [ ptraceIfFalse "A7" $ pfromData foldD.overcommitted #>= 0
                  , ptraceIfFalse "A8" $ time #< contributionEndTime
                  , ptraceIfFalse "A9" $ plengthBS # (pfromData $ pfstBuiltin # key) #== pconstant expectedPkhLength
                  , ptraceIfFalse "A10" $ pbetween (pconstant $ minNodeIndex - 1) (pfromData $ psndBuiltin # key) (pconstant $ maxNodeIndex + 1)
                  ]
          PDNothing _ ->
            pand'List
              [ ptraceIfFalse "A11" $ pfromData foldD.overcommitted #== 0
              , ptraceIfFalse "A12" $ pisDNothing # foldD.cutoffKey
              ]
      , ptraceIfFalse "A13" $ pisDNothing # (nodeD.key)
      ]

commitFoldMintingPolicy :: CommitFoldPolicyConfig -> Script
commitFoldMintingPolicy cfg = toScript $ pcommitFoldMintingPolicy # pconstant cfg

commitFoldMintingPolicySymbol :: CommitFoldPolicyConfig -> CurrencySymbol
commitFoldMintingPolicySymbol cfg = CurrencySymbol $ getScriptHash $ scriptHash (commitFoldMintingPolicy cfg)

commitFoldPolicyScript :: Script
commitFoldPolicyScript = toScript pcommitFoldMintingPolicy

commitFoldPolicySymbol :: CurrencySymbol
commitFoldPolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash commitFoldPolicyScript
