{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Launchpad.CommitFold where

import Launchpad.Constants
import Launchpad.Types
import Plutarch
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.PlutusScript
import Plutarch.Positive (ptryPositive)
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Types.Classes
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Commit Fold Validator

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.

     Note: daoAdmin should not be equal to commitFoldOwner, otherwise possible compensation double-satisfaction is introduced
-}
data CommitFoldConfig = CommitFoldConfig
  { starter :: TxOutRef
  , commitFoldSymbol :: CurrencySymbol
  , nodeSymbol :: CurrencySymbol
  , withdrawalEndTime :: POSIXTime
  , daoAdmin :: PubKeyHash
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''CommitFoldConfig [('CommitFoldConfig, 0)]
PlutusTx.makeLift ''CommitFoldConfig

data PCommitFoldConfig (s :: S)
  = PCommitFoldConfig
      ( Term
          s
          ( PDataRecord
              [ "starter" ':= PTxOutRef
              , "commitFoldSymbol" ':= PCurrencySymbol
              , "nodeSymbol" ':= PCurrencySymbol
              , "withdrawalEndTime" ':= PPOSIXTime
              , "daoAdmin" ':= PPubKeyHash
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PCommitFoldConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PCommitFoldConfig where
  type PLifted PCommitFoldConfig = CommitFoldConfig

deriving via
  (DerivePConstantViaData CommitFoldConfig PCommitFoldConfig)
  instance
    (PConstantDecl CommitFoldConfig)

pisCommitFoldValueCorrect :: Term s (PCurrencySymbol :--> PTokenName :--> PValue 'Sorted 'Positive :--> PBool)
pisCommitFoldValueCorrect = phoistAcyclic $ plam \foldCs foldTn foldValue ->
  pand'List
    [ pvalueOf # foldValue # foldCs # foldTn #== 1
    , plength # ((pto . pto) foldValue) #== 2
    , pall # plam (\pair -> plength # (pto . pto . pfromData) (psndBuiltin # pair) #== 1) # (pto . pto) foldValue
    ]

commitFoldValidator :: Term s (PCommitFoldConfig :--> PValidator)
commitFoldValidator = plam \cfg datum redeemer context ->
  let datum' = ptryFrom @PCommitFoldDatum datum fst
      redeemer' = ptryFrom @PCommitFoldRedeemer redeemer fst
   in popaque (perrorIfFalse #$ ptraceIfFalse "G0" $ pvalidateCommitFold cfg datum' redeemer' context)

{- | Get the commitment of the node.
Fully eligible nodes return the full commitment.
The partially eligible node returns the commitment up to the cutoff.
Ineligible nodes return 0.
-}
pnodeCommitment ::
  HRecOf PNode '["next", "key", "committed", "createdTime"] s ->
  Term s (PMaybeData (PAsData PNodeKey)) ->
  Term s (PMaybeData (PAsData PPOSIXTime)) ->
  Term s (PMaybeData (PAsData PNodeKey)) ->
  Term s PInteger ->
  Term s PInteger
pnodeCommitment node nodeKey cutoffTime cutoffKey overcommitted =
  pmatch cutoffTime \case
    PDNothing _ -> node.committed
    PDJust timeR -> plet (pfromData . pfromData $ pfield @"_0" # timeR) \time -> plet (pfromData $ pfromDJust # cutoffKey) \key ->
      pcond
        [ (node.createdTime #< time, node.committed)
        , (node.createdTime #> time, 0)
        ]
        ( plet (pfromData $ pfromDJust # nodeKey) \nodeKey' ->
            pcond
              [ (pbuiltinPairLess # nodeKey' # key, node.committed)
              , (pbuiltinPairLess # key # nodeKey', 0)
              ]
              (pto $ ptryPositive #$ node.committed - overcommitted)
        )

pnextCommitState :: Term s (PCurrencySymbol :--> PCommitFoldScott :--> PTxOut :--> PCommitFoldScott)
pnextCommitState = phoistAcyclic $ plam \nodeSymbol state node -> unTermCont do
  nodeF <- pletFieldsC @'["value", "datum", "address"] node

  stateF <- pmatchC state

  nodeD <- pletFieldsC @'["next", "key", "committed", "createdTime"] (pfromPDatum @PNode # (ptryFromInlineDatum # nodeF.datum))
  nodeKey <- pletC nodeD.key

  pguardC "G1" $
    pand'List
      [ ptraceIfFalse "G2" $ pscriptHashFromAddress # nodeF.address #== pjust stateF.nodeScriptHash
      , ptraceIfFalse "G3" $ nodeKey #== stateF.next
      , ptraceIfFalse "G4" $ pvalueOf # nodeF.value # nodeSymbol # pscriptHashToTokenName stateF.nodeScriptHash #== 1
      , ptraceIfFalse "G5" $ pisDJust # nodeKey
      ]
  pure
    ( pcon @PCommitFoldScott
        stateF
          { next = pfromData nodeD.next
          , committed =
              stateF.committed + pnodeCommitment nodeD nodeKey stateF.cutoffTime stateF.cutoffKey stateF.overcommitted
          , nodeCount = stateF.nodeCount + 1
          }
    )

{- | The commit fold application validation.

  - a valid on-chain linked list [A_1, ..., A_n] in the reference inputs
  - [A_1, ..., A_n] in the reference inputs have key of type Just
  - commit fold K is spent, its validator is run
  - K utxo has one commit fold token with its token name equal to the script hash of the commit fold validator
  - the next field of K is a Just value
  - the next field of K is equal to the key field of A_1
  - K' utxo is created
  - the address of K' is equal to the address of K
  - the value of K' is equal to the value of K
  - the next field of K' is the next field of A_n
  - the nodeScriptHash field of K' is equal to the nodeScriptHash field of K
  - the nodeCount field of K' is equal to the nodeCount field of K plus the number of processed nodes
  - the owner field of K' is equal to the owner field of K
  - tx is signed by the commit fold owner
  - the committed field of K' is equal to the sum of the committed field of K and the commitment of the folded eligible nodes
  - the overcommitted field of K' is equal to the overcommitted field of K
  - the cutoffTime field of K' is equal to the cutoffTime field of K
  - the cutoffKey field of K' is equal to the cutoffKey field of K
-}
pvalidateCommitFoldApplication ::
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PScriptHash ->
  Term s PTxOut ->
  Term s PCommitFoldDatum ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PBool
pvalidateCommitFoldApplication nodeCs selfCs selfValidatorHash ownInput selfDatum outputs referenceInputs signatories nodeIndices =
  unTermCont do
    outFold <-
      pletC
        ( passertSingleSpecificInput "G6"
            # plam id
            # selfValidatorHash
            # selfCs
            # pscriptHashToTokenName selfValidatorHash
            # outputs
        )
    outFoldF <- pletFieldsC @'["value", "datum"] outFold
    outFoldD <- pletC (pfromPDatum @PCommitFoldDatum # (ptryFromInlineDatum # outFoldF.datum))

    let ownValue = pfield @"value" # ownInput

    selfState <- pletC (toScott selfDatum)
    selfStateF <- pmatchC selfState
    let expectedOutState =
          ( pfoldl
              # (pnextCommitState # nodeCs)
              # selfState
              # (pmap # plam (\i -> ptxInInfoResolved # (pelemAt # pfromData i # referenceInputs)) # nodeIndices)
          )
        expectedOutDatum = fromScott expectedOutState

    pure $
      pand'List
        [ ptraceIfFalse "G7" $ outFoldF.value #== ownValue
        , ptraceIfFalse "G8" $ pnot # (pnull # nodeIndices)
        , ptraceIfFalse "G9" $ pisSignedByPubKeyAddress selfStateF.owner signatories
        , ptraceIfFalse "G10" $ outFoldD #== expectedOutDatum
        , ptraceIfFalse "G11" $ pisDJust # selfStateF.next
        ]

pvalidateCommitFold ::
  Term s PCommitFoldConfig -> Term s PCommitFoldDatum -> Term s PCommitFoldRedeemer -> Term s PScriptContext -> Term s PBool
pvalidateCommitFold cfg datum redeemer context = unTermCont do
  cfgF <- pletFieldsC @'["commitFoldSymbol", "nodeSymbol", "withdrawalEndTime", "daoAdmin"] cfg

  contextF <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "redeemers", "mint", "validRange", "signatories"] contextF.txInfo
  df <- pletFieldsC @'["nodeScriptHash", "owner"] datum
  nodeScriptHash <- pletC df.nodeScriptHash
  owner <- pletC df.owner

  ownInput <- pletC (pownInput # contextF.purpose # tx.inputs)
  selfCs <- pletC cfgF.commitFoldSymbol

  let selfValidatorHash = pgetValidatorHashFromScriptAddress #$ ptxOutAddress # ownInput

  pure $
    pand'List
      [ ptraceIfFalse "G12" $ ptxOutHasAssociatedToken selfCs ownInput
      , ptraceIfFalse "G13" $ pcountScriptInputs # selfValidatorHash # tx.inputs #== 1
      , pmatch redeemer \case
          PCommitFold r ->
            pvalidateCommitFoldApplication
              cfgF.nodeSymbol
              selfCs
              selfValidatorHash
              ownInput
              datum
              tx.outputs
              tx.referenceInputs
              tx.signatories
              (pfield @"nodes" # r)
          -- commit fold K is spent, its validator is run
          -- K utxo has one commit fold token with its token name equal to the script hash of the commit fold validator
          -- there is one input with its validator hash equal to the nodeScriptHash field of K
          --   and its redeemer is either start the rewards fold or fail the launchpad
          PDelegateCommitToNode _ ->
            pany
              # plam
                ( \i ->
                    (ppaysToCredential # nodeScriptHash # (ptxInInfoResolved # i))
                      #&& plet
                        (ptryFrom @PNodeRedeemer (pto (ptryTxOutRefRedeemer # (pfield @"outRef" # i) # tx.redeemers)) fst)
                        ( (flip pmatch)
                            \case
                              PStartRewardsFold _ -> ptrue
                              PFailLaunchpad _ -> ptrue
                              _ -> pfalse
                        )
                )
              # tx.inputs
          -- commit fold K is spent, its validator is run
          -- there is only one script utxo in the inputs
          -- 1 commit fold token is burned with its token name equal to the script hash of the commit fold validator
          -- the only currency symbol being burned is the commit fold symbol (equal to 2, because mint always contains 0 ADA)
          -- at least "emergencyWithdrawalPeriod" amount of time has passed since the withdrawalEndTime
          -- transaction is signed by the commit fold owner
          PCommitFoldEmergencyWithdrawal _ -> unTermCont do
            PTimestamps lowerTime _ <- pmatchC (pfiniteTxValidityRangeTimestamps # tx.validRange)
            pure $
              pand'List
                [ ptraceIfFalse "G14" $ pcountAllScriptInputs # tx.inputs #== 1
                , ptraceIfFalse "G15" $ pvalueOf # tx.mint # selfCs # pscriptHashToTokenName selfValidatorHash #== -1
                , ptraceIfFalse "G16" $ plength # ((pto . pto) (pfromData tx.mint)) #== 2
                , ptraceIfFalse "G17" $ pto (lowerTime - cfgF.withdrawalEndTime) #> pconstant emergencyWithdrawalPeriod
                , ptraceIfFalse "G18" $ ptxSignedByPkh # (paddressPubKeyCredential # owner) # tx.signatories
                ]
      ]

commitFoldScriptValidator :: CommitFoldConfig -> Script
commitFoldScriptValidator cfg = toScript (commitFoldValidator # pconstant cfg)

commitFoldScriptValidatorHash :: CommitFoldConfig -> ScriptHash
commitFoldScriptValidatorHash = scriptHash . commitFoldScriptValidator

commitFoldScriptAddress :: CommitFoldConfig -> Address
commitFoldScriptAddress = scriptHashToAddress . commitFoldScriptValidatorHash

commitFoldScript :: Script
commitFoldScript = toScript commitFoldValidator
