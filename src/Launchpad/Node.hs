{-# LANGUAGE BlockArguments #-}

module Launchpad.Node where

import Launchpad.Constants
import Launchpad.Types
import Launchpad.Util
import Plutarch
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
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
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Node Validator

     "oilAda" should be equal to (commitFoldFeeAda + rewardsFoldFeeAda + oilAda)

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.

     Note: daoAdmin should not be equal to commitFoldOwner, otherwise possible compensation double-satisfaction is introduced
-}
data NodeConfig = NodeConfig
  { starter :: TxOutRef
  , nodeSymbol :: CurrencySymbol
  , rewardsFoldSymbol :: CurrencySymbol
  , rewardsFoldValidatorHash :: ScriptHash
  , commitFoldSymbol :: CurrencySymbol
  , commitFoldValidatorHash :: ScriptHash
  , tokensHolderSymbol :: CurrencySymbol
  , tokensHolderValidatorHash :: ScriptHash
  , failProofSymbol :: CurrencySymbol
  , failProofValidatorHash :: ScriptHash
  , presaleTierCs :: CurrencySymbol
  , presaleTierMinCommitment :: Integer
  , presaleTierMaxCommitment :: Integer
  , presaleTierStartTime :: POSIXTime
  , defaultTierMinCommitment :: Integer
  , defaultTierMaxCommitment :: Integer
  , defaultStartTime :: POSIXTime
  , startTime :: POSIXTime
  , contributionEndTime :: POSIXTime
  , withdrawalEndTime :: POSIXTime
  , projectMinCommitment :: Integer
  , projectMaxCommitment :: Integer
  , totalTokens :: Integer
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , owner :: Address
  , daoAdmin :: PubKeyHash
  , daoFeeReceiver :: Address
  , collateral :: Integer
  , nodeAda :: Integer
  , oilAda :: Integer
  , commitFoldFeeAda :: Integer
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''NodeConfig [('NodeConfig, 0)]
PlutusTx.makeLift ''NodeConfig

data PNodeConfig (s :: S)
  = PNodeConfig
      ( Term
          s
          ( PDataRecord
              [ "starter" ':= PTxOutRef
              , "nodeSymbol" ':= PCurrencySymbol
              , "rewardsFoldSymbol" ':= PCurrencySymbol
              , "rewardsFoldValidatorHash" ':= PScriptHash
              , "commitFoldSymbol" ':= PCurrencySymbol
              , "commitFoldValidatorHash" ':= PScriptHash
              , "tokensHolderSymbol" ':= PCurrencySymbol
              , "tokensHolderValidatorHash" ':= PScriptHash
              , "failProofSymbol" ':= PCurrencySymbol
              , "failProofValidatorHash" ':= PScriptHash
              , "presaleTierCs" ':= PCurrencySymbol
              , "presaleTierMinCommitment" ':= PInteger
              , "presaleTierMaxCommitment" ':= PInteger
              , "presaleTierStartTime" ':= PPOSIXTime
              , "defaultTierMinCommitment" ':= PInteger
              , "defaultTierMaxCommitment" ':= PInteger
              , "defaultStartTime" ':= PPOSIXTime
              , "startTime" ':= PPOSIXTime
              , "contributionEndTime" ':= PPOSIXTime
              , "withdrawalEndTime" ':= PPOSIXTime
              , "projectMinCommitment" ':= PInteger
              , "projectMaxCommitment" ':= PInteger
              , "totalTokens" ':= PInteger
              , "projectSymbol" ':= PCurrencySymbol
              , "projectToken" ':= PTokenName
              , "raisingSymbol" ':= PCurrencySymbol
              , "raisingToken" ':= PTokenName
              , "owner" ':= PAddress
              , "daoAdmin" ':= PPubKeyHash
              , "daoFeeReceiver" ':= PAddress
              , "collateral" ':= PInteger
              , "nodeAda" ':= PInteger
              , "oilAda" ':= PInteger
              , "commitFoldFeeAda" ':= PInteger
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow, PIsData, PDataFields)

instance DerivePlutusType PNodeConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNodeConfig where
  type PLifted PNodeConfig = NodeConfig

deriving via
  (DerivePConstantViaData NodeConfig PNodeConfig)
  instance
    (PConstantDecl NodeConfig)

nodeValidator :: Term s (PNodeConfig :--> PValidator)
nodeValidator = phoistAcyclic $ plam \cfg rawDatum rawRedeemer context ->
  let datum = ptryFrom @PNode rawDatum fst
      redeemer = ptryFrom @PNodeRedeemer rawRedeemer fst
   in popaque $ perrorIfFalse #$ ptraceIfFalse "H0" $ pnodeScriptValidator cfg datum redeemer context

pdelegateToFold :: Term s (PScriptHash :--> PScriptHash :--> (PDatum :--> PScriptHash) :--> PBuiltinList PTxInInfo :--> PInteger :--> PBool)
pdelegateToFold = phoistAcyclic $ plam \selfValidatorHash foldValidatorHash foldNodeHashField inputs foldIndex ->
  plet (ptxInInfoResolved # (pelemAt # foldIndex # inputs)) \o ->
    (ppaysToCredential # foldValidatorHash # o) #&& (foldNodeHashField # (ptryFromInlineDatum # (ptxOutDatum # o)) #== selfValidatorHash)

{- | Validate the removal of the next node.

  - 2 list element utxos A and B are spent, where A's validator is being run
  - A utxo has one list element token with its token name equal to the script hash of the list element validator
  - the redeemer of B is the remove the current element redeemer
-}
pvalidateNextNodeRemoval ::
  Term s PTxOutRef ->
  Term s PScriptHash ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PMap 'Unsorted PScriptPurpose PRedeemer) ->
  Term s PBool
pvalidateNextNodeRemoval selfTxOutRef selfValidatorHash inputs redeemers = plet
  (pfilter # plam (\i -> ppaysToCredential # selfValidatorHash # (ptxInInfoResolved # i)) # inputs)
  \nodes ->
    let otherNode = passertSingleton "H1" # (pfilter # plam (\i -> (pfield @"outRef" # i) #/= selfTxOutRef) # nodes)
     in pand'List
          [ ptraceIfFalse "H2" $ plength # nodes #== 2
          , ptraceIfFalse "H3" $ pisRemoveCurrentNodeConstructor (ptryTxOutRefRedeemer # (pfield @"outRef" # otherNode) # redeemers)
          ]

{- | Validate the removal of the current node.

  - the transaction is signed by the owner of the element (extracted from the key field)
  - 2 list element utxos A and B are spent, where B's validator is being run
  - B utxo has one list element token with its token name equal to the script hash of the list element validator
  - A' utxo is created (the only created node)
  - the upper time range limit is < the withdrawal end time of the launchpad config
  - the difference between the lower time range limit and the createdTime field of B is at least 5 minutes
  - the list element token of B is burned
  - next field of A' is Just (next field of B)
  - A' has the same datum as A apart from the next field
  - A' has the same value as A
  - A' has the same address as A
  - the next field of A is Just (key field of B)
-}
pvalidateCurrentNodeRemoval ::
  Term s PPOSIXTime ->
  Term s PCurrencySymbol ->
  Term s PNode ->
  Term s PTxOutRef ->
  Term s PScriptHash ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  (Term s PPOSIXTime, Term s PPOSIXTime) ->
  Term s PBool
pvalidateCurrentNodeRemoval withdrawalEndTime nodeCs selfDatum selfTxOutRef selfValidatorHash signatories inputs outputs mint (lowerTime, upperTime) =
  pletFields @'["key", "next", "createdTime"] selfDatum \selfF ->
    unTermCont do
      inputNodes <- pletC (passertDoubleton (pfilter # plam (\i -> ppaysToCredential # selfValidatorHash # (ptxInInfoResolved # i)) # inputs))
      outputNode <- pletC (passertSingleton "H4" # (pfindScriptOutputsWithAddress # selfValidatorHash # outputs))
      pure $
        pand'List
          [ ptraceIfFalse "H5" $ ptxSignedBy # signatories # pdata (pcon (PPubKeyHash (pfromData (pfstBuiltin # (pfromData (pfromDJust # selfF.key))))))
          , ptraceIfFalse "H6" $ pisMintingExactAmountForPolicy # (-1) # nodeCs # pscriptHashToTokenName selfValidatorHash # mint
          , ptraceIfFalse "H7" $ upperTime #< withdrawalEndTime
          , ptraceIfFalse "H8" $ pnodesRemovalCheck selfTxOutRef lowerTime selfF inputNodes outputNode
          ]

pnodesRemovalCheck ::
  Term s PTxOutRef ->
  Term s PPOSIXTime ->
  HRecOf PNode '["key", "next", "createdTime"] s ->
  Term s (PPair PTxInInfo PTxInInfo) ->
  Term s (PTriplet PAddress (PValue 'Sorted 'Positive) POutputDatum) ->
  Term s PBool
pnodesRemovalCheck selfTxOutRef lowerTime self inputNodes outputNode = unTermCont do
  let otherNode =
        pmatch inputNodes \(PPair leftNode rightNode) ->
          pif (pfromData (pfield @"outRef" # leftNode) #== selfTxOutRef) rightNode leftNode

  otherF <- pletFieldsC @'["address", "datum", "value"] (ptxInInfoResolved # otherNode)
  other <-
    pletFieldsC @'["key", "next", "committed", "createdTime"]
      (pfromPDatum @PNode # (ptryFromInlineDatum # otherF.datum))

  PTriplet outAddress outValue outDatum <- pmatchC outputNode
  out <- pletFieldsC @'["key", "next", "committed", "createdTime"] (pfromPDatum @PNode # (ptryFromInlineDatum # outDatum))

  pure $
    pand'List
      [ ptraceIfFalse "H9" $ self.key #== other.next
      , ptraceIfFalse "H10" $ other.key #== out.key
      , ptraceIfFalse "H11" $ other.committed #== out.committed
      , ptraceIfFalse "H12" $ other.createdTime #== out.createdTime
      , ptraceIfFalse "H13" $ out.next #== self.next
      , ptraceIfFalse "H14" $ otherF.value #== outValue
      , ptraceIfFalse "H15" $ otherF.address #== outAddress
      , ptraceIfFalse "H16" $ pto (lowerTime - self.createdTime) #> pconstant nodesInactivityPeriod
      ]

{- | The validator for the new node insertion.

  - the upper time range limit is < than the contribution end time of the launchpad config
  - the lower time range limit is > than the start time of the used tier of the launchpad config
  - there is one input list element utxo A, its validator is being run
  - A utxo has one list element token with its token name equal to the script hash of the list element validator
  - B utxo is created
  - createdTime of B is equal to the upper bound of the current time approximation
  - value of B is some value D of expected committed tokens + node ADA + 1 list element token with the correct token name + 1 tier-specific token if the tier is not default
  - the committed field of B is equal to D
  - D is >= than the min commitment of the tier of the launchpad config
  - D is <= than the max commitment of the tier of the launchpad config
  - the transaction is signed by the owner of the element extracted from the first part of the key
  - when next field of A is Just G, the key of B must be < than G
  - key of A must be < than the key of B
  - next field of B is the next field of A
  - B has the same script address as A
  - A' utxo is created
  - A' has the same value, address, and datum as A apart from the next field
  - the next field of A' is Just (key field of B)
  - the index of the key of B is in range [0, 1_000]
  - A' is assumed to be provided before B in the outputs
  - project token holder utxo T is in the reference inputs
  - value of T is token holder token with correct token name
-}
pvalidateNodeInsertion ::
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PInteger, Term s PInteger, Term s PPOSIXTime) ->
  (Term s PCurrencySymbol, Term s PInteger, Term s PInteger, Term s PPOSIXTime) ->
  Term s PAddress ->
  Term s PPOSIXTime ->
  Term s PCurrencySymbol ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PTxOut ->
  Term s PScriptHash ->
  Term s PNode ->
  Term s PTier ->
  (Term s PPOSIXTime, Term s PPOSIXTime) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PBool
pvalidateNodeInsertion
  (raisingSymbol, raisingToken)
  (defaultTierMinCommitment, defaultTierMaxCommitment, defaultStartTime)
  (presaleTierCs, presaleTierMinCommitment, presaleTierMaxCommitment, presaleTierStartTime)
  ownAddress
  contributionEndTime
  projectTokenHolderCs
  projectTokenHolderValidatorHash
  nodeCs
  nodeAda
  selfTxOut
  selfValidatorHash
  node
  tier
  (lowerTime, upperTime)
  outputs
  inputs
  referenceInputs
  signatories = unTermCont do
    PPair selfOut newOut <- pmatchC (passertDoubleton (pfindScriptOutputsWithAddress # selfValidatorHash # outputs))
    PTriplet selfOutAddress selfOutValue selfOutDatum <- pmatchC selfOut
    PTriplet _ newOutValue newOutDatum <- pmatchC newOut

    newOutD <- pletFieldsC @'["key", "next", "committed", "createdTime"] (pfromPDatum @PNode # (ptryFromInlineDatum # newOutDatum))
    newOutCommitted <- pletC newOutD.committed
    newKey <- pletC (pfromData (pfromDJust # newOutD.key))

    selfOutD <- pletFieldsC @'["key", "next", "committed", "createdTime"] (pfromPDatum @PNode # (ptryFromInlineDatum # selfOutDatum))

    selfInD <- pletFieldsC @'["key", "next", "committed", "createdTime"] node
    selfInKey <- pletC selfInD.key
    selfInValue <- pletC (pfield @"value" # selfTxOut)

    PPair p tierStartTime <-
      pmatchC
        ( ptierParams
            tier
            (defaultTierMinCommitment, defaultTierMaxCommitment, defaultStartTime)
            (presaleTierMinCommitment, presaleTierMaxCommitment, presaleTierStartTime)
        )
    PPair tierMinCommitment tierMaxCommitment <- pmatchC p

    let tokenHolderUtxo =
          passertSingleSpecificInput "H17"
            # ptxInInfoResolved
            # projectTokenHolderValidatorHash
            # projectTokenHolderCs
            # pscriptHashToTokenName projectTokenHolderValidatorHash
            # referenceInputs

    -- Enforces only one node input
    _ <- pmatchC (ptryUniqueScriptTxInInfo selfValidatorHash inputs)
    pure $
      pand'List
        [ ptraceIfFalse "H18" $ selfOutValue #== selfInValue
        , ptraceIfFalse "H19" $ ownAddress #== selfOutAddress
        , ptraceIfFalse "H20" $ selfInKey #== selfOutD.key
        , ptraceIfFalse "H21" $ selfInD.committed #== selfOutD.committed
        , ptraceIfFalse "H22" $ selfInD.createdTime #== selfOutD.createdTime
        , ptraceIfFalse "H23" $ selfOutD.next #== newOutD.key
        , ptraceIfFalse "H24" $ newOutD.createdTime #== upperTime
        , ptraceIfFalse "H25" $ newOutD.next #== selfInD.next
        , ptraceIfFalse "H26" $ pbetween (pconstant $ minNodeIndex - 1) (pfromData $ psndBuiltin # newKey) (pconstant $ maxNodeIndex + 1)
        , pcheckProjectTokenHolder # tokenHolderUtxo # selfValidatorHash # projectTokenHolderCs # projectTokenHolderValidatorHash
        , pmatch selfInKey \case
            PDJust r -> ptraceIfFalse "H27" $ pbuiltinPairLess # pfromData (pfield @"_0" # r) # newKey
            PDNothing _ -> ptrue
        , pmatch selfInD.next \case
            PDJust r -> ptraceIfFalse "H28" $ pbuiltinPairLess # newKey # pfromData (pfield @"_0" # r)
            PDNothing _ -> ptrue
        , ptraceIfFalse "H29" $
            pisNodeValueCorrect
              # raisingSymbol
              # raisingToken
              # presaleTierCs
              # nodeCs
              # pscriptHashToTokenName selfValidatorHash
              # tier
              # newOutValue
              # newOutCommitted
              # nodeAda
        , ptraceIfFalse "H30" $ ptxSignedBy # signatories # pdata (pcon (PPubKeyHash (pfromData (pfstBuiltin # newKey))))
        , ptraceIfFalse "H31" $ lowerTime #> tierStartTime
        , ptraceIfFalse "H32" $ upperTime #< contributionEndTime
        , ptraceIfFalse "H33" $ newOutCommitted #>= tierMinCommitment
        , ptraceIfFalse "H34" $ newOutCommitted #<= tierMaxCommitment
        ]

pcheckProjectTokenHolder :: Term s (PTxOut :--> PScriptHash :--> PCurrencySymbol :--> PScriptHash :--> PBool)
pcheckProjectTokenHolder = phoistAcyclic $ plam $ \tokenHolderUtxo selfValidatorHash projectTokenHolderCs projectTokenHolderValidatorHash -> unTermCont do
  tokenHolderF <- pletFieldsC @'["datum", "value"] tokenHolderUtxo
  let nodeScriptHash =
        pfromData (pto (pfromPDatum @PLaunchpadTokensHolderDatum # (ptryFromInlineDatum # tokenHolderF.datum)))
  pure $
    pand'List
      [ ptraceIfFalse "H35" $ nodeScriptHash #== selfValidatorHash
      , ptraceIfFalse "H36" $ pvalueOf # tokenHolderF.value # projectTokenHolderCs # pscriptHashToTokenName projectTokenHolderValidatorHash #== 1
      ]

{- | The validator for the insertion of multiple separator nodes.

  - the upper time range limit is < than the start time of the launchpad config
  - there is one input list element utxo A, its validator is being run
  - A utxo has one list element token with its token name equal to the script hash of the list element validator
  - a list of node elements utxos [A', A'1, ..., A'n] is created on the outputs located starting at the offset
    provided in the redeemer and continuing to the end of the outputs list
  - the number of output nodes must be equal to the number of processed separator nodes plus one recreated node
  - [A', A'1, ..., A'n] are sorted by their keys and form a valid onchain list
  - A has the same datum (apart from the next key) and value as A'
  - [A'1, ..., A'n] are all spacer nodes with 0 committed tokens in the datum and the value, oil ada, 1 list element validity token
    and keys which have their length of the first part the keys equal to 1,
    the created times must be equal to the upper bound of the current time approximation,
    the indices of the keys must be in the range of [0, 1_000]
  - next field of A'n is equal to the next field of A
  - the transaction is signed by either the launchpad owner or by the Dao team
-}
pvalidateSeparatorsInsertion ::
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PPubKeyHash ->
  Term s PPubKeyHash ->
  Term s PPOSIXTime ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PInteger ->
  Term s PNode ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PScriptHash ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PPOSIXTime ->
  Term s PBool
pvalidateSeparatorsInsertion
  (raisingSymbol, raisingToken)
  launchpadOwner
  daoAdmin
  startTime
  inputs
  outputs
  nodesOffset
  selfDatum
  nodeCs
  nodeAda
  nodeValidatorHash
  signatories
  upperTime = unTermCont do
    let selfOut =
          passertSingleSpecificInput "H37"
            # ptxInInfoResolved
            # nodeValidatorHash
            # nodeCs
            # pscriptHashToTokenName nodeValidatorHash
            # inputs
    selfF <- pletFieldsC @'["address", "value"] selfOut
    self <- pletFieldsC @'["key", "next", "committed", "createdTime"] selfDatum
    let nodes = pdrop' # nodesOffset # outputs
    pure
      ( pelimList
          ( \recreatedNodeOut separatorNodes -> unTermCont do
              recreatedNodeF <- pletFieldsC @'["address", "datum", "value"] recreatedNodeOut
              recreatedNode <-
                pletFieldsC
                  @'["key", "next", "committed", "createdTime"]
                  (pfromPDatum @PNode # (ptryFromInlineDatum # recreatedNodeF.datum))
              let foldedNodes =
                    pfoldl
                      # (pcheckSeparatorNode # raisingSymbol # raisingToken # nodeCs # nodeAda # nodeValidatorHash # upperTime)
                      # (ppair recreatedNode.next 0)
                      # separatorNodes
              PPair lastNext processedSeparators <- pmatchC foldedNodes

              pure
                ( pand'List
                    [ ptraceIfFalse "H38" $
                        (ptxSignedBy # signatories # pdata launchpadOwner)
                          #|| (ptxSignedBy # signatories # pdata daoAdmin)
                    , ptraceIfFalse "H39" $ upperTime #< startTime
                    , ptraceIfFalse "H40" $ recreatedNode.committed #== self.committed
                    , ptraceIfFalse "H41" $ recreatedNode.createdTime #== self.createdTime
                    , ptraceIfFalse "H42" $ recreatedNode.key #== self.key
                    , ptraceIfFalse "H43" $ recreatedNodeF.value #== selfF.value
                    , ptraceIfFalse "H44" $ recreatedNodeF.address #== selfF.address
                    , ptraceIfFalse "H45" $ self.next #== lastNext
                    , ptraceIfFalse "H46" $ (processedSeparators + 1) #== pcountScriptOutputs # nodeValidatorHash # outputs
                    ]
                )
          )
          pfalse -- if the list is empty, the validation fails
          nodes
      )

-- | Checks that the separator node is valid. Must be used in a fold over the separator nodes.
pcheckSeparatorNode ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PInteger
        :--> PScriptHash
        :--> PPOSIXTime
        :--> PPair (PMaybeData (PAsData PNodeKey)) PInteger
        :--> PTxOut
        :--> PPair (PMaybeData (PAsData PNodeKey)) PInteger
    )
pcheckSeparatorNode =
  phoistAcyclic $
    plam
      \raisingSymbol
       raisingToken
       nodeCs
       nodeAda
       nodeValidatorHash
       upperTime
       foldState
       nodeOut -> unTermCont do
          PPair prevNext processedNodes <- pmatchC foldState
          nodeF <- pletFieldsC @'["datum", "value", "address"] nodeOut
          nodeD <- pletFieldsC @'["key", "next", "committed", "createdTime"] (pfromPDatum @PNode # (ptryFromInlineDatum # nodeF.datum))
          nodeKey <- pletC nodeD.key
          justNodeKey <- pletC (pfromData (pfromDJust # nodeKey))
          nodeNext <- pletC nodeD.next
          pguardC
            "H47"
            ( pand'List
                [ ptraceIfFalse "H48" $ pfromData nodeD.committed #== 0
                , ptraceIfFalse "H49" $ nodeD.createdTime #== upperTime
                , ptraceIfFalse "H50" $ nodeKey #== prevNext
                , pmatch nodeNext \case
                    PDJust next -> ptraceIfFalse "H51" $ pbuiltinPairLess # justNodeKey # pfromData (pfield @"_0" # next)
                    PDNothing _ -> ptrue
                , ptraceIfFalse "H52" $ plengthBS # pfromData (pfstBuiltin # justNodeKey) #== pconstant separatorNodeKeyLength -- 1 byte
                , ptraceIfFalse "H53" $ pbetween (pconstant $ minNodeIndex - 1) (pfromData $ psndBuiltin # justNodeKey) (pconstant $ maxNodeIndex + 1)
                , -- NOTE: the tier is the default one, so the passed tier symbol are ignored
                  ptraceIfFalse "H54" $
                    pisNodeValueCorrect
                      # raisingSymbol
                      # raisingToken
                      # pconstant ""
                      # nodeCs
                      # pscriptHashToTokenName nodeValidatorHash
                      # pcon PDefault
                      # nodeF.value
                      # 0
                      # nodeAda
                , ptraceIfFalse "H55" $ pscriptHashFromAddress # nodeF.address #== pjust nodeValidatorHash
                ]
            )
          pure (ppair nodeNext (processedNodes + 1))

{- | Validation of starting the rewards fold

  - 1 list element utxo A is consumed, its validator is run
  - A utxo has one list element token with its token name equal to the script hash of the list element validator
  - A key field is Nothing
  - 1 list element token is burned with its token name equal to the script hash of the list element validator
  - 1 commit fold utxo K is consumed
  - 1 commit fold token is burned, its token name is equal to the script hash of the commit fold validator
  - nodeScriptHash field of K is equal to the script hash of the list element validator
  - next field of K is Nothing
  - next field of A is a Just value
  - 1 rewards fold utxo R 0,1 is created
  - R 0,1 has 1 rewards fold token with its token name equal to the script hash of the rewards fold validator
  - nodeScriptHash field of R 0,1 is equal to the script hash of the list element validator
  - next field of R 0,1 is equal to the next field of A
  - commitFoldOwner field of R 0,1 is equal to the owner field of K
  - there is at least oilAda ADA deposited in the rewards fold utxo
  - R 0,1 holds 2 unique tokens, ADA and rewards fold token
  - when the cutoffTime field of K is Nothing, the committed field of K is smaller than the max commitment of the launchpad config
  - when the cutoffTime field of K is a Just value, the committed field of K is equal to the max commitment of the launchpad config
  - cutoffKey field of R 0,1 is equal to the cutoffKey field of K
  - cutoffTime field of R 0,1 is equal to the cutoffTime field of K
  - committed field of R 0,1 is equal to the committed field of K
  - overcommitted field of R 0,1 is equal to the overcommitted field of K
-}
pvalidateStartRewardsFold ::
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PScriptHash ->
  Term s PNode ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PValue 'Sorted guarantees) ->
  Term s PBool
pvalidateStartRewardsFold
  minCommitment
  maxCommitment
  oilAda
  nodeCs
  selfValidatorHash
  commitFoldCs
  commitFoldValidatorHash
  rewardsFoldCs
  rewardsFoldValidatorHash
  node
  inputs
  outputs
  mint = unTermCont do
    let commitFoldUtxo =
          passertSingleSpecificInput "H56"
            # ptxInInfoResolved
            # commitFoldValidatorHash
            # commitFoldCs
            # pscriptHashToTokenName commitFoldValidatorHash
            # inputs
        rewardsFoldUtxo =
          passertSingleSpecificInput "H57"
            # plam id
            # rewardsFoldValidatorHash
            # rewardsFoldCs
            # pscriptHashToTokenName rewardsFoldValidatorHash
            # outputs

    nodeD <- pletFieldsC @'["next", "key"] node

    commitFoldF <- pletFieldsC @'["datum"] commitFoldUtxo
    commitFoldD' <- pletC $ pfromPDatum @PCommitFoldDatum # (ptryFromInlineDatum # commitFoldF.datum)
    commitFoldD <- pletFieldsC @'["nodeScriptHash", "next", "committed", "overcommitted", "cutoffKey", "cutoffTime", "owner"] commitFoldD'

    rewardsFoldF <- pletFieldsC @'["datum", "value"] rewardsFoldUtxo
    rewardsFoldD' <- pletC $ pfromPDatum @PRewardsFoldDatum # (ptryFromInlineDatum # rewardsFoldF.datum)
    rewardsFoldD <- pletFieldsC @'["nodeScriptHash", "cutoffKey", "cutoffTime", "next", "committed", "overcommitted", "commitFoldOwner"] rewardsFoldD'

    pure $
      pand'List
        [ ptraceIfFalse "H70" $ pisDNothing # nodeD.key
        , ptraceIfFalse "H71" $ pisDJust # nodeD.next
        , ptraceIfFalse "H72" $ pvalueOf # mint # nodeCs # pscriptHashToTokenName selfValidatorHash #== (-1)
        , ptraceIfFalse "H73" $ pvalueOf # mint # commitFoldCs # pscriptHashToTokenName commitFoldValidatorHash #== (-1)
        , ptraceIfFalse "H74" $ pvalueOf # rewardsFoldF.value # padaSymbol # padaToken #>= oilAda
        , ptraceIfFalse "H75" $ pcountOfUniqueTokens # rewardsFoldF.value #== 2
        , ptraceIfFalse "H58" $ rewardsFoldD.committed #== commitFoldD.committed
        , ptraceIfFalse "H59" $ rewardsFoldD.overcommitted #== commitFoldD.overcommitted
        , ptraceIfFalse "H60" $ rewardsFoldD.cutoffKey #== commitFoldD.cutoffKey
        , ptraceIfFalse "H61" $ rewardsFoldD.cutoffTime #== commitFoldD.cutoffTime
        , ptraceIfFalse "H62" $ rewardsFoldD.commitFoldOwner #== commitFoldD.owner
        , ptraceIfFalse "H63" $ rewardsFoldD.next #== nodeD.next
        , ptraceIfFalse "H64" $ rewardsFoldD.nodeScriptHash #== selfValidatorHash
        , pmatch rewardsFoldD.cutoffTime \case
            PDJust _ -> ptraceIfFalse "H65" $ rewardsFoldD.committed #== maxCommitment
            PDNothing _ ->
              pand'List
                [ ptraceIfFalse "H66" $ rewardsFoldD.committed #>= minCommitment
                , ptraceIfFalse "H67" $ rewardsFoldD.committed #< maxCommitment
                ]
        , ptraceIfFalse "H68" $ commitFoldD.nodeScriptHash #== selfValidatorHash
        , ptraceIfFalse "H69" $ pisDNothing # commitFoldD.next
        ]

{- | Validate the launchpad failure.

  - There are three script inputs
  - There is a commit fold input
  - The nodeScriptHash field of the commit fold is equal to the node script hash
  - The next field of the commit fold is Nothing
  - The key field of the node is Nothing
  - One node token is burned
  - One commit fold token is burned
  - One project tokens holder token is burned
  - There is a fail proof output with a fail proof token
  - The datum of the fail proof output is a script hash of the node
  - The fail proof output has two unique tokens
  - There is an output paying totalTokens of the project tokens to the launchpad owner, it has two unique tokens
  - There is an output paying min(nodeCount * 2, collateral) ada to the commit fold owner, it has 1 unique token
  - If there is any collateral left, there is an output paying the remaining collateral to the dao fee receiver, it has one unique token
  - The cutoffTime field of the commit fold is Nothing
  - The committed field of the commit fold is smaller than the min commitment of the project
-}
pvalidateLaunchpadFailure ::
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  Term s PNode ->
  Term s PInteger ->
  Term s PAddress ->
  Term s PAddress ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  (Term s (PBuiltinList PTxInInfo), Term s (PBuiltinList PTxOut)) ->
  Term s (PValue anyKey anyAmount) ->
  Term s PBool
pvalidateLaunchpadFailure
  (nodeCs, selfValidatorHash)
  node
  minCommitment
  owner
  daoFeeReceiver
  totalTokens
  collateral
  commitFoldFeeAda
  (projectCs, projectTn)
  (projectTokensHolderCs, projectTokensHolderValidatorHash)
  (commitFoldCs, commitFoldValidatorHash)
  (failProofCs, failProofValidatorHash)
  (inputs, outputs)
  mint = unTermCont do
    nodeD <- pletFieldsC @'["key"] node

    let commitFoldUtxo =
          passertSingleSpecificInput "H76"
            # ptxInInfoResolved
            # commitFoldValidatorHash
            # commitFoldCs
            # pscriptHashToTokenName commitFoldValidatorHash
            # inputs
    commitFoldF <- pletFieldsC @'["datum"] commitFoldUtxo
    commitFoldD' <- pletC $ pfromPDatum @PCommitFoldDatum # (ptryFromInlineDatum # commitFoldF.datum)

    let failProofUtxo =
          passertSingleSpecificInput "H77"
            # pid
            # failProofValidatorHash
            # failProofCs
            # pscriptHashToTokenName failProofValidatorHash
            # outputs
    failProofF <- pletFieldsC @'["value", "datum"] failProofUtxo
    PFailProofDatum failProofStoredNodeHash <-
      pmatchC (pfromPDatum @PFailProofDatum # (ptryFromInlineDatum # failProofF.datum))

    -- NOTE: this is inlined and not hoisted, and it's not a closed term as it references the outputs
    let isAddressCompensated address compensation =
          pany
            # plam
              ( \o -> unTermCont do
                  oF <- pletFieldsC @'["address", "value"] o
                  pure $
                    (oF.address #== address)
                      #&& pand'List
                        [ pcountOfUniqueTokens # oF.value #== 1
                        , pvalueOf # oF.value # padaSymbol # padaToken #>= compensation
                        ]
              )
            # outputs
        -- The dao is compensated with the collateral if the expected compensation is more than 0
        isDaoCompensated daoCollateral =
          pif (daoCollateral #> 0) (isAddressCompensated daoFeeReceiver daoCollateral) ptrue
        isCommitFoldOwnerCompensated commitFoldCollateral commitFoldOwner =
          isAddressCompensated commitFoldOwner commitFoldCollateral
    commitFoldD <-
      pletFieldsC
        @'[ "nodeScriptHash"
          , "next"
          , "committed"
          , "overcommitted"
          , "cutoffKey"
          , "cutoffTime"
          , "nodeCount"
          , "owner"
          ]
        commitFoldD'
    -- In a rare case when the expected commit fold compensation is bigger than the locked collateral,
    -- the commit fold is compensated with the locked collateral and the dao collateral is set to 0.
    commitFoldCollateral <- pletC (pmin # (commitFoldD.nodeCount * commitFoldFeeAda) # collateral)
    daoCollateral <- pletC (collateral - commitFoldCollateral)

    pure $
      pand'List
        [ ptraceIfFalse "H79" $ commitFoldD.nodeScriptHash #== selfValidatorHash
        , ptraceIfFalse "H80" $ pisDNothing # commitFoldD.next
        , ptraceIfFalse "H81" $ pisDNothing # commitFoldD.cutoffTime
        , ptraceIfFalse "H82" $ pfromData commitFoldD.committed #< minCommitment
        , ptraceIfFalse "H90" $ isDaoCompensated daoCollateral
        , ptraceIfFalse "H91" $ isCommitFoldOwnerCompensated commitFoldCollateral commitFoldD.owner
        , -- head node, commit fold, project tokens holder
          ptraceIfFalse "H78" $ pcountAllScriptInputs # inputs #== 3
        , ptraceIfFalse "H83" $ pisDNothing # nodeD.key
        , ptraceIfFalse "H84" $ pvalueOf # mint # nodeCs # pscriptHashToTokenName selfValidatorHash #== (-1)
        , ptraceIfFalse "H85" $ pvalueOf # mint # commitFoldCs # pscriptHashToTokenName commitFoldValidatorHash #== (-1)
        , ptraceIfFalse "H86" $ pvalueOf # mint # projectTokensHolderCs # pscriptHashToTokenName projectTokensHolderValidatorHash #== (-1)
        , ptraceIfFalse "H87" $ pfromData failProofStoredNodeHash #== selfValidatorHash
        , -- ada and fail proof token
          ptraceIfFalse "H88" $ pcountOfUniqueTokens # failProofF.value #== 2
        , -- The owner's project tokens are returned
          ptraceIfFalse "H89" $
            pany
              # plam
                ( \o -> unTermCont do
                    oF <- pletFieldsC @'["address", "value"] o
                    pure $
                      (oF.address #== owner)
                        #&& pand'List
                          [ -- ada and project tokens
                            pcountOfUniqueTokens # oF.value #== 2
                          , pvalueOf # oF.value # projectCs # projectTn #== totalTokens
                          ]
                )
              # outputs
        ]

{- | Reclamation of the node when the launchpad has failed.

  - The number of burned node tokens is equal to the number of input nodes
  - If the node is a separator, the transaction is signed by the dao
  - If the node is not a separator, the transaction is signed by the node owner
  - There is a fail proof utxo with the node script hash as the datum and the fail proof token in the reference inputs
-}
preclaimNode ::
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  Term s (PAsData PPubKeyHash) ->
  Term s PNode ->
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PValue anyKey anyAmount) ->
  Term s PBool
preclaimNode (nodeCs, selfValidatorHash) admin node (failProofCs, failProofValidatorHash) inputs referenceInputs signatories mint = unTermCont do
  nodeF <- pletFieldsC @'["key", "committed"] node
  let nodeOwner = (pcon . PPubKeyHash . pfromData) (pfstBuiltin #$ pfromData $ pfromDJust # nodeF.key)
      isSeparator = pfromData nodeF.committed #== 0
      inputNodes = pcountScriptInputs # selfValidatorHash # inputs
  pure $
    pand'List
      [ ptraceIfFalse "H92" $
          pany
            # plam
              ( \i -> pletFields @'["value", "address", "datum"] (ptxInInfoResolved # i) \iF ->
                  (pscriptHashFromAddress # iF.address #== pjust failProofValidatorHash)
                    #&& pand'List
                      [ pfromData (pto (pfromPDatum @PFailProofDatum # (ptryFromInlineDatum # iF.datum))) #== selfValidatorHash
                      , (pvalueOf # iF.value # failProofCs # pscriptHashToTokenName failProofValidatorHash #== 1)
                      ]
              )
            # referenceInputs
      , ptraceIfFalse "H93" $ pvalueOf # mint # nodeCs # pscriptHashToTokenName selfValidatorHash #== (-inputNodes)
      , ptraceIfFalse "H94" $
          pif
            isSeparator
            (ptxSignedBy # signatories # admin)
            (ptxSignedBy # signatories # pdata nodeOwner)
      ]

{- | Validation of emergency withdrawal

  - 1 list element utxo is consumed, its validator is run
  - there is only one script utxo in the inputs
  - the only currency symbol being burned is the node symbol (equal to 2, because mint always contains 0 ADA)
  - 1 list element token is burned with its token name equal to the script hash of the list element validator
  - at least "emergencyWithdrawalPeriod" amount of time has passed since the withdrawalEndTime
  - transaction is signed by:
    - if commitment = 0 (separator node) a dao signature is required
    - otherwise a node owner signature is required
-}
pnodeEmergencyWithdrawal ::
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PNode ->
  Term s PPOSIXTime ->
  Term s PPubKeyHash ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PPOSIXTime ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PBool
pnodeEmergencyWithdrawal selfValidatorHash nodeSymbol node lowerTime daoAdmin inputs mint withdrawalEndTime signatories = unTermCont do
  nodeF <- pletFieldsC @'["key", "committed"] node
  pure $
    pand'List
      [ ptraceIfFalse "H95" $ pcountAllScriptInputs # inputs #== 1
      , ptraceIfFalse "H96" $ pvalueOf # mint # nodeSymbol # pscriptHashToTokenName selfValidatorHash #== -1
      , ptraceIfFalse "H97" $ plength # ((pto . pto) mint) #== 2
      , ptraceIfFalse "H98" $ pto (lowerTime - withdrawalEndTime) #> pconstant emergencyWithdrawalPeriod
      , ptraceIfFalse "H99" $
          pif
            (pfromData nodeF.committed #== 0)
            (ptxSignedByPkh # pdata daoAdmin # signatories)
            (ptxSignedByPkh # pdata (pcon . PPubKeyHash . pfromData $ pfstBuiltin #$ pfromData $ pfromDJust # nodeF.key) # signatories)
      ]

pnodeScriptValidator :: Term s PNodeConfig -> Term s PNode -> Term s PNodeRedeemer -> Term s PScriptContext -> Term s PBool
pnodeScriptValidator cfg node redeemer context = unTermCont do
  cfgF <-
    pletFieldsC
      @'[ "nodeSymbol"
        , "commitFoldSymbol"
        , "commitFoldValidatorHash"
        , "rewardsFoldSymbol"
        , "rewardsFoldValidatorHash"
        , "tokensHolderSymbol"
        , "tokensHolderValidatorHash"
        , "failProofSymbol"
        , "failProofValidatorHash"
        , "defaultTierMinCommitment"
        , "defaultTierMaxCommitment"
        , "presaleTierMinCommitment"
        , "presaleTierMaxCommitment"
        , "raisingSymbol"
        , "raisingToken"
        , "projectSymbol"
        , "projectToken"
        , "presaleTierCs"
        , "presaleTierStartTime"
        , "defaultStartTime"
        , "startTime"
        , "contributionEndTime"
        , "withdrawalEndTime"
        , "projectMinCommitment"
        , "projectMaxCommitment"
        , "totalTokens"
        , "owner"
        , "daoAdmin"
        , "daoFeeReceiver"
        , "collateral"
        , "nodeAda"
        , "oilAda"
        , "commitFoldFeeAda"
        ]
      cfg

  contextFields <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["inputs", "referenceInputs", "outputs", "redeemers", "signatories", "mint", "validRange"] contextFields.txInfo
  ownInput <- pletC (pownInput # contextFields.purpose # tx.inputs)
  ownAddress <- pletC (pfield @"address" # ownInput)
  let selfTxOutRef = pownRef contextFields.purpose
      timestamps = pfiniteTxValidityRangeTimestamps # tx.validRange

  nodeCs <- pletC cfgF.nodeSymbol
  selfValidatorHash <- pletC (pgetValidatorHashFromScriptAddress #$ ptxOutAddress # ownInput)

  commitFoldCs <- pletC cfgF.commitFoldSymbol
  commitFoldValidatorHash <- pletC cfgF.commitFoldValidatorHash
  rewardsFoldCs <- pletC cfgF.rewardsFoldSymbol
  rewardsFoldValidatorHash <- pletC cfgF.rewardsFoldValidatorHash
  projectTokenHolderCs <- pletC cfgF.tokensHolderSymbol
  projectTokenHolderValidatorHash <- pletC cfgF.tokensHolderValidatorHash
  failProofCs <- pletC cfgF.failProofSymbol
  failProofValidatorHash <- pletC cfgF.failProofValidatorHash

  inputs <- pletC tx.inputs
  referenceInputs <- pletC tx.referenceInputs
  outputs <- pletC tx.outputs
  redeemers <- pletC tx.redeemers
  signatories <- pletC tx.signatories
  mint <- pletC tx.mint
  PTimestamps lowerTime upperTime <- pmatchC timestamps

  pure $
    pand'List
      [ ptraceIfFalse "H100" $ ptxOutHasAssociatedToken nodeCs ownInput
      , pmatch redeemer \case
          PInsertNode r ->
            ptraceIfFalse "H101" $
              pvalidateNodeInsertion
                (cfgF.raisingSymbol, cfgF.raisingToken)
                (cfgF.defaultTierMinCommitment, cfgF.defaultTierMaxCommitment, cfgF.defaultStartTime)
                (cfgF.presaleTierCs, cfgF.presaleTierMinCommitment, cfgF.presaleTierMaxCommitment, cfgF.presaleTierStartTime)
                ownAddress
                cfgF.contributionEndTime
                projectTokenHolderCs
                projectTokenHolderValidatorHash
                nodeCs
                cfgF.nodeAda
                ownInput
                selfValidatorHash
                node
                (pfield @"tier" # r)
                (lowerTime, upperTime)
                outputs
                inputs
                referenceInputs
                signatories
          PInsertSeparators r ->
            ptraceIfFalse "H102" $
              pvalidateSeparatorsInsertion
                (cfgF.raisingSymbol, cfgF.raisingToken)
                (paddressPubKeyCredential # cfgF.owner)
                cfgF.daoAdmin
                cfgF.startTime
                inputs
                outputs
                (pfield @"offset" # r)
                node
                nodeCs
                cfgF.nodeAda
                selfValidatorHash
                signatories
                upperTime
          PRemoveCurrentNode _ ->
            ptraceIfFalse "H103" $
              pvalidateCurrentNodeRemoval
                cfgF.withdrawalEndTime
                nodeCs
                node
                selfTxOutRef
                selfValidatorHash
                signatories
                inputs
                outputs
                mint
                (lowerTime, upperTime)
          PRemoveNextNode _ -> ptraceIfFalse "H104" $ pvalidateNextNodeRemoval selfTxOutRef selfValidatorHash inputs redeemers
          PStartRewardsFold _ ->
            ptraceIfFalse "H105" $
              pvalidateStartRewardsFold
                cfgF.projectMinCommitment
                cfgF.projectMaxCommitment
                cfgF.oilAda
                nodeCs
                selfValidatorHash
                commitFoldCs
                commitFoldValidatorHash
                rewardsFoldCs
                rewardsFoldValidatorHash
                node
                inputs
                outputs
                mint
          PFailLaunchpad _ ->
            ptraceIfFalse "H106" $
              pvalidateLaunchpadFailure
                (nodeCs, selfValidatorHash)
                node
                cfgF.projectMinCommitment
                cfgF.owner
                cfgF.daoFeeReceiver
                cfgF.totalTokens
                cfgF.collateral
                cfgF.commitFoldFeeAda
                (cfgF.projectSymbol, cfgF.projectToken)
                (projectTokenHolderCs, projectTokenHolderValidatorHash)
                (commitFoldCs, commitFoldValidatorHash)
                (failProofCs, failProofValidatorHash)
                (inputs, outputs)
                mint
          PDelegateToRewardsFold index ->
            let foldNodeHash d = pfromData $ pfield @"nodeScriptHash" # (pfromPDatum @PRewardsFoldDatum # d)
             in ptraceIfFalse "H107" $
                  pdelegateToFold
                    # selfValidatorHash
                    # rewardsFoldValidatorHash
                    # plam foldNodeHash
                    # inputs
                    # (pfield @"foldIndex" # index)
          PReclaimAfterFailure _ ->
            ptraceIfFalse "H108" $
              preclaimNode
                (nodeCs, selfValidatorHash)
                cfgF.daoAdmin
                node
                (failProofCs, failProofValidatorHash)
                inputs
                referenceInputs
                signatories
                mint
          PNodeEmergencyWithdrawal _ ->
            ptraceIfFalse "H109" $
              pnodeEmergencyWithdrawal
                selfValidatorHash
                nodeCs
                node
                lowerTime
                cfgF.daoAdmin
                inputs
                mint
                cfgF.withdrawalEndTime
                signatories
      ]

nodeScriptValidator :: NodeConfig -> Script
nodeScriptValidator cfg = toScript (nodeValidator # pconstant cfg)

nodeScriptValidatorHash :: NodeConfig -> ScriptHash
nodeScriptValidatorHash = scriptHash . nodeScriptValidator

nodeScriptAddress :: NodeConfig -> Address
nodeScriptAddress = scriptHashToAddress . nodeScriptValidatorHash

nodeScript :: Script
nodeScript = toScript nodeValidator
