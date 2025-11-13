{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.Node where

import Launchpad.Types
import Plutarch
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Extra.Value
import Plutarch.Lift
import Plutarch.Mint.Util
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Node Minting Policy

     "nodeAda" should be equal to (commitFoldFeeAda + rewardsFoldFeeAda + oilAda)

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data NodePolicyConfig = NodePolicyConfig
  { starter :: TxOutRef
  , owner :: PubKeyHash
  , nodeAda :: Integer
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''NodePolicyConfig [('NodePolicyConfig, 0)]
PlutusTx.makeLift ''NodePolicyConfig

data PNodePolicyConfig (s :: S)
  = PNodePolicyConfig
      ( Term
          s
          ( PDataRecord
              [ "starter" ':= PTxOutRef
              , "owner" ':= PPubKeyHash
              , "nodeAda" ':= PInteger
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PNodePolicyConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNodePolicyConfig where
  type PLifted PNodePolicyConfig = NodePolicyConfig

deriving via
  (DerivePConstantViaData NodePolicyConfig PNodePolicyConfig)
  instance
    (PConstantDecl NodePolicyConfig)

pnodeMintingPolicy :: Term s (PNodePolicyConfig :--> PMintingPolicy)
pnodeMintingPolicy = phoistAcyclic $ plam \cfg _redeemer context ->
  popaque $ perrorIfFalse #$ ptraceIfFalse "C0" $ pnodeMintingPolicyTyped # cfg # context

{- | The list element minting policy.

There are four supported use-cases:
 - Minting the head of the list
 - Adding a new element to the list
 - Adding one or more separator nodes to the list
 - Removing an element from the list


Minting the head of the list involves the following validations:
 - one list element token is minted
 - an output (node) is created with the minted token
 - output (node) holds 2 unique tokens, ADA and list element token
 - the token name of the minted token is equal to the script hash of the node on which it sits
 - the node's key field is set to nothing
 - the node's next field is set to nothing
 - the node's createdTime field is set to the upper bound of the current time approximation
 - the committed field is set to 0
 - the value of the node utxo is equal to 1 node token & at least oil ADA
 - the transaction is signed by the launchpad owner
 - the starter utxo is spent


Adding a new element to the list involves the following validations:
 - one list element token is minted
 - there are two output utxos with the validator hash equal to the token name of the minted token,
   which both hold list element tokens with token name equal to their validator has
 - there is an input with the validator hash equal to the token name of the minted token

Adding one or more separator nodes to the list involves the following:
  - one or more list element tokens are minted
  - the same number + 1 of node outputs are created
  - the token name of the minted token is equal to the script hashes of the utxos on which the tokens sit
  - there is an input with the validator hash equal to the token name of the minted token

Removing an element is from the list covers both the user-initiated removal and the rewards fold.
It involves the following validations:
 - at least one list element token is burned
 - there is an input with the validator hash equal to the token name of the burned token
-}
pnodeMintingPolicyTyped :: Term s (PNodePolicyConfig :--> PScriptContext :--> PBool)
pnodeMintingPolicyTyped = phoistAcyclic $ plam \cfg context -> unTermCont do
  contextFields <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["inputs", "outputs", "mint", "signatories", "validRange"] contextFields.txInfo
  nodeCs <- pletC (pownCurrencySymbol contextFields.purpose)
  PPair nodeTn nodeMinted <- pmatchC (pvalueOfSingleton tx.mint nodeCs)
  pure $
    pif
      (nodeMinted #< 0)
      (pvalidateRemoval nodeTn tx.inputs)
      (pvalidateNodeMinting cfg nodeMinted nodeCs nodeTn tx.signatories tx.validRange tx.inputs tx.outputs)

pvalidateNodeMinting ::
  Term s PNodePolicyConfig ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PPOSIXTimeRange ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PBool
pvalidateNodeMinting cfg nodeMinted nodeCs nodeTn signatories validRange inputs outputs =
  plet (pfindScriptOutputs # ptokenNameAsScriptHash nodeTn # outputs) \scriptOutputs ->
    plet (plength # scriptOutputs) \scriptOutputsLength ->
      pif
        (scriptOutputsLength #== 1)
        ( plet (phead # scriptOutputs) \nodeInfo ->
            pletFields @'["starter", "owner", "nodeAda"] cfg \cfgF ->
              pvalidateCreation
                nodeMinted
                cfgF.starter
                cfgF.owner
                cfgF.nodeAda
                nodeCs
                nodeTn
                signatories
                (pfiniteTxValidityRangeTimestamps # validRange)
                inputs
                nodeInfo
        )
        (pvalidateInsertion (nodeMinted, scriptOutputsLength) nodeCs nodeTn inputs scriptOutputs)

pvalidateCreation ::
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PTimestamps ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PPair (PValue 'Sorted 'Positive) POutputDatum) ->
  Term s PBool
pvalidateCreation nodeMinted starter owner nodeAda nodeCs nodeTn signatories validRange inputs nodeInfo =
  pmatch validRange \(PTimestamps _ upper) ->
    pand'List
      [ ptraceIfFalse "C1" $ pisUTXOSpent # starter # inputs
      , ptraceIfFalse "C2" $ ptxSignedBy # signatories # owner
      , pinitialHeadNodeIsRight nodeCs nodeTn nodeAda upper nodeInfo
      , ptraceIfFalse "C3" $ nodeMinted #== 1
      ]

pinitialHeadNodeIsRight ::
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s PPOSIXTime ->
  Term s (PPair (PValue 'Sorted 'Positive) POutputDatum) ->
  Term s PBool
pinitialHeadNodeIsRight nodeCs nodeTn nodeAda currentTime nodeInfo = pmatch nodeInfo \(PPair value datum) ->
  pand'List
    [ pinitialHeadDatumIsRight currentTime (pfromPDatum @PNode # (ptryFromInlineDatum # datum))
    , pinitialHeadValueIsRight nodeCs nodeTn value nodeAda
    ]

pinitialHeadDatumIsRight :: Term s PPOSIXTime -> Term s PNode -> Term s PBool
pinitialHeadDatumIsRight currentTime datum = pletFields @'["key", "next", "committed", "createdTime"] datum \d ->
  pand'List
    [ ptraceIfFalse "C4" $ pisDNothing # d.key
    , ptraceIfFalse "C5" $ pisDNothing # d.next
    , ptraceIfFalse "C6" $ pfromData d.committed #== 0
    , ptraceIfFalse "C7" $ d.createdTime #== currentTime
    ]

pinitialHeadValueIsRight :: Term s PCurrencySymbol -> Term s PTokenName -> Term s (PValue 'Sorted 'Positive) -> Term s PInteger -> Term s PBool
pinitialHeadValueIsRight nodeCs nodeTn value nodeAda =
  pand'List
    [ ptraceIfFalse "C8" $ pvalueOf # value # nodeCs # nodeTn #== 1
    , ptraceIfFalse "C9" $ padaOf # value #>= nodeAda
    , ptraceIfFalse "C10" $ pcountOfUniqueTokens # value #== 2
    ]

pvalidateInsertion ::
  (Term s PInteger, Term s PInteger) ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PList (PPair (PValue 'Sorted 'Positive) POutputDatum)) ->
  Term s PBool
pvalidateInsertion (nodeMinted, nodeScriptOutputs) selfCs selfTn inputs nodeOutputs =
  pand'List
    [ ptraceIfFalse "C11" $ pdelegateToAssociatedValidator selfTn inputs
    , ptraceIfFalse "C12" $ (nodeMinted + 1) #== nodeScriptOutputs
    , ptraceIfFalse "C13" $ pall # plam (\o -> pvalueOf # (pfst # o) # selfCs # selfTn #== 1) # nodeOutputs
    ]

pvalidateRemoval :: Term s PTokenName -> Term s (PBuiltinList PTxInInfo) -> Term s PBool
pvalidateRemoval = pdelegateToAssociatedValidator

nodeMintingPolicy :: NodePolicyConfig -> Script
nodeMintingPolicy cfg = toScript $ pnodeMintingPolicy # pconstant cfg

nodeMintingPolicySymbol :: NodePolicyConfig -> CurrencySymbol
nodeMintingPolicySymbol cfg = CurrencySymbol $ getScriptHash $ scriptHash (nodeMintingPolicy cfg)

nodePolicyScript :: Script
nodePolicyScript = toScript pnodeMintingPolicy

nodePolicySymbol :: CurrencySymbol
nodePolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash nodePolicyScript
