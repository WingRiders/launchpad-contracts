{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Integration.Launchpad.Node where

import Data.Maybe
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util
import Launchpad.Mint.Node qualified as N
import Launchpad.Node qualified as N
import Launchpad.Types
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Value (assetClass, assetClassValue, assetClassValueOf)
import PlutusLedgerApi.V1.Value qualified as V
import PlutusLedgerApi.V2
import PlutusTx.Monoid (inv)

data MaliciousNodeAction
  = None
  | SmallerKey
  | NoTokensHolderRef
  | NoMinting
  | DoubleMinting
  | WrongSignature
  | ChangeStakingCredential

data NodeActionTiming
  = AfterContributionPhase
  | AfterWithdrawalPhase
  deriving (Eq)

findNodeUtxo :: Maybe NodeKey -> [(TxOutRef, TxOut)] -> (TxOutRef, TxOut)
findNodeUtxo nodeKey nodes = case filter f nodes of
  [a] -> a
  _ -> error "findNodeUtxo: no node found"
  where
    f (_, TxOut _ _ (OutputDatum (Datum d)) _) = fmap key (fromBuiltinData d) == Just nodeKey
    f _ = False

createHeadNode :: LaunchpadConfig -> PubKeyHash -> AddToken -> Run ()
createHeadNode config wallet = createNode config wallet (Node Nothing Nothing 0 0)

createTierNode :: LaunchpadConfig -> PubKeyHash -> Node -> Tier -> AddToken -> Run ()
createTierNode config wallet datum tier addToken = do
  let value =
        adaValue nodeAdaAmount
          <> case addToken of
            AddToken ->
              assetClassValue
                ( assetClass
                    (N.nodeMintingPolicySymbol (nodePolicyConfig config))
                    (scriptHashToTokenName (N.nodeScriptValidatorHash (nodeConfig config)))
                )
                1
            SkipToken -> mempty
          <> assetClassValue config.raisingToken datum.committed
          <> tierToken config tier
  usp <- spend wallet value
  ensureTx wallet (createNodeTx config usp datum value)

createNode :: LaunchpadConfig -> PubKeyHash -> Node -> AddToken -> Run ()
createNode config wallet datum = createTierNode config wallet datum Default

createNodeTx :: LaunchpadConfig -> UserSpend -> Node -> Value -> Tx
createNodeTx config usp datum value =
  mconcat
    [ userSpend usp
    , payToScript (nodeValidator config) (InlineDatum datum) value
    ]

createNodeRefScript :: LaunchpadConfig -> PubKeyHash -> Run ()
createNodeRefScript config wallet = do
  usp <- spend wallet (adaValue 1)
  ensureTx wallet (loadRefScript (nodeValidator config) (adaValue 1) <> userSpend usp)

removeNode :: LaunchpadConfig -> PubKeyHash -> MaliciousNodeAction -> (Maybe NodeKey, Maybe NodeKey) -> Run ()
removeNode config wallet maliciousAction (remainingKey, removedKey) = do
  nodes <- utxoAt (nodeValidator config)
  [(nodeRef, _)] <- refScriptAt (nodeValidator config)
  let oldNodeUtxo = findNodeUtxo remainingKey nodes
      removedNodeUtxo = findNodeUtxo removedKey nodes
  range <- currentTimeRad 1_001
  submitTx wallet
    =<< signTx
      ( case maliciousAction of
          WrongSignature -> wallet
          _ -> PubKeyHash . fst . fromJust $ removedKey
      )
    =<< validateIn range (removeNodeTx config maliciousAction nodeRef (oldNodeUtxo, removedNodeUtxo))

removeNodeTx :: LaunchpadConfig -> MaliciousNodeAction -> TxOutRef -> ((TxOutRef, TxOut), (TxOutRef, TxOut)) -> Tx
removeNodeTx config maliciousAction nodeRef (oldNodeUtxo, removedNodeUtxo) =
  mconcat
    [ spendScriptRef nodeRef (nodeValidator config) (fst oldNodeUtxo) RemoveNextNode oldNode
    , spendScriptRef nodeRef (nodeValidator config) (fst removedNodeUtxo) RemoveCurrentNode removedNode
    , case maliciousAction of
        ChangeStakingCredential -> payToScript (appendStakingCredential mockStakingCredential (nodeValidator config)) (InlineDatum updatedOldNode) (txOutValue (snd oldNodeUtxo))
        _ -> payToScript (nodeValidator config) (InlineDatum updatedOldNode) (txOutValue (snd oldNodeUtxo))
    , memptyIf V.isZero burnValue (mintValue (nodeMintingPolicy config) ())
    , payToKey (PubKeyHash . fst . fromJust $ removedNode.key) owedToOwner
    ]
  where
    oldNode = txOutputDatum (snd oldNodeUtxo)
    removedNode = txOutputDatum (snd removedNodeUtxo)
    updatedOldNode = (oldNode :: Node) {next = removedNode.next}
    burnedNodeToken =
      singleton
        (N.nodeMintingPolicySymbol (nodePolicyConfig config))
        (scriptHashToTokenName (N.nodeScriptValidatorHash (nodeConfig config)))
        (-1)
    burnValue = case maliciousAction of
      NoMinting -> mempty
      _ -> burnedNodeToken
    owedToOwner =
      raisingTokenValue
        <> if config.raisingToken == adaAssetClass
          then mempty
          else
            adaAssetValue <> case maliciousAction of
              NoMinting -> inv burnedNodeToken
              _ -> mempty
    adaAssetValue = ada $ adaOf removedNodeValue
    raisingTokenValue = assetClassValue config.raisingToken raisingTokenCount
    raisingTokenCount = assetClassValueOf removedNodeValue config.raisingToken
    removedNodeValue = txOutValue . snd $ removedNodeUtxo

insertSeparatorNodes :: LaunchpadConfig -> PubKeyHash -> MaliciousNodeAction -> PubKeyHash -> Maybe NodeKey -> ([POSIXTime -> Node]) -> Run ()
insertSeparatorNodes config wallet maliciousAction signer oldKey separators = do
  oldNodeUtxos <- utxoAt (nodeValidator config)
  let oldNodeUtxo = findNodeUtxo oldKey oldNodeUtxos
  [(nodeRef, _)] <- refScriptAt (nodeValidator config)
  range <- currentTimeRad 1_001
  let actualNodes = case range of
        Interval _ (UpperBound (Finite t) _) -> map ($ t) separators
        _ -> error "insertSeparatorNodes: invalid range"
  usp <- spend wallet $ adaValue (sum (map (\n -> nodeAdaAmount + n.committed) actualNodes))
  tx <- validateIn range (insertSeparatorNodesTx config usp nodeRef oldNodeUtxo actualNodes)
  submitTx wallet
    =<< ( case maliciousAction of
            WrongSignature -> pure tx
            _ -> signTx signer tx
        )

insertSeparatorNodesTx :: LaunchpadConfig -> UserSpend -> TxOutRef -> (TxOutRef, TxOut) -> [Node] -> Tx
insertSeparatorNodesTx config usp nodeRef oldNodeUtxo actualNodes =
  mconcat $
    [ -- skip the change
      spendScriptRef nodeRef (nodeValidator config) (fst oldNodeUtxo) (InsertSeparators 1) oldNode
    , userSpend usp
    , payToScript (nodeValidator config) (InlineDatum updatedOldNode) (txOutValue (snd oldNodeUtxo))
    , mconcat $
        map
          ( \n ->
              payToScript
                (nodeValidator config)
                (InlineDatum n)
                (adaValue (n.committed + nodeAdaAmount) <> newNodeToken)
          )
          actualNodes
    , memptyIf V.isZero (mconcat (map (const newNodeToken) actualNodes)) (mintValue (nodeMintingPolicy config) ())
    ]
  where
    oldNode = txOutputDatum (snd oldNodeUtxo)
    updatedOldNode = (oldNode :: Node) {next = key (head actualNodes)}
    newNodeToken =
      singleton
        (scriptCurrencySymbol (nodeMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
        1

insertNode :: LaunchpadConfig -> PubKeyHash -> MaliciousNodeAction -> Maybe NodeKey -> (POSIXTime -> Node) -> Tier -> Run ()
insertNode config wallet maliciousAction oldKey newNode tier = do
  [(nodeRef, _)] <- refScriptAt (nodeValidator config)
  oldNodeUtxos <- utxoAt (nodeValidator config)
  let oldNodeUtxo = findNodeUtxo oldKey oldNodeUtxos
  range <- currentTimeRad 1_001 -- how does this translate to the actual time in the valid range of the context?
  let actualNode = case range of
        Interval _ (UpperBound (Finite t) _) -> newNode t
        _ -> error "insertNode: invalid range"
      value = (adaValue nodeAdaAmount <> tierToken config tier <> assetClassValue config.raisingToken actualNode.committed)
  [firstTokensHolder] <- filter (isNothing . txOutReferenceScript . snd) <$> utxoAt (projectTokensHolderFirstValidator config)
  usp <- spend wallet (adaValue (nodeAdaAmount) <> tierToken config tier <> assetClassValue config.raisingToken actualNode.committed)
  submitTx wallet
    =<< signTx (maybe wallet (PubKeyHash . fst) actualNode.key)
    =<< validateIn range (insertNodeTx config usp nodeRef oldNodeUtxo firstTokensHolder actualNode value tier maliciousAction)

insertNodeTx :: LaunchpadConfig -> UserSpend -> TxOutRef -> (TxOutRef, TxOut) -> (TxOutRef, TxOut) -> Node -> Value -> Tier -> MaliciousNodeAction -> Tx
insertNodeTx config usp nodeRef oldNodeUtxo firstTokensHolder newNode nodeValue tier maliciousAction =
  mconcat $
    [ spendScriptRef nodeRef (nodeValidator config) (fst oldNodeUtxo) (InsertNode tier) oldNode
    , -- NOTE: the order of the outputs is reversed compared to the order here
      payToScript
        (nodeValidator config)
        (InlineDatum newNode)
        (nodeToken <> nodeValue)
    , case maliciousAction of
        ChangeStakingCredential -> payToScript (appendStakingCredential mockStakingCredential (nodeValidator config)) (InlineDatum updatedOldNode) (txOutValue (snd oldNodeUtxo))
        _ -> payToScript (nodeValidator config) (InlineDatum updatedOldNode) (txOutValue (snd oldNodeUtxo))
    , case maliciousAction of
        DoubleMinting ->
          mconcat
            [ memptyIf V.isZero (nodeToken <> nodeToken) (mintValue (nodeMintingPolicy config) ())
            , payToKey config.daoAdmin nodeToken
            ]
        _ -> memptyIf V.isZero nodeToken (mintValue (nodeMintingPolicy config) ())
    , userSpend usp
    , refInputInline (fst firstTokensHolder)
    ]
  where
    oldNode = txOutputDatum (snd oldNodeUtxo)
    updatedOldNode = (oldNode :: Node) {next = newNode.key}
    nodeToken = case maliciousAction of
      NoMinting -> mempty
      _ ->
        singleton
          (scriptCurrencySymbol (nodeMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
          1

data ReclamationScenario = UserReclaims | AdminReclaims

reclaimNode :: LaunchpadConfig -> ReclamationScenario -> PubKeyHash -> NodeKey -> Run ()
reclaimNode config scenario wallet nodeKey = do
  [(nodeRef, _)] <- refScriptAt (nodeValidator config)
  nodeUtxos <- utxoAt (nodeValidator config)
  let nodeUtxo = findNodeUtxo (Just nodeKey) nodeUtxos
  [(failProofRef, _)] <- utxoAt (failProofValidator config)
  range <- currentTimeRad 1_001
  let signer = case scenario of
        UserReclaims -> PubKeyHash (fst nodeKey)
        AdminReclaims -> config.daoAdmin
  submitTx wallet =<< signTx signer =<< validateIn range (reclaimNodeTx config nodeRef nodeUtxo failProofRef signer)

reclaimNodeTx :: LaunchpadConfig -> TxOutRef -> (TxOutRef, TxOut) -> TxOutRef -> PubKeyHash -> Tx
reclaimNodeTx config nodeRef (nodeTxOutRef, nodeOut) failProofRef wallet =
  mconcat
    [ spendScriptRef nodeRef (nodeValidator config) nodeTxOutRef ReclaimAfterFailure (txOutputDatum nodeOut)
    , payToKey wallet (txOutValue nodeOut <> burnedNodeToken)
    , refInputInline failProofRef
    , mintValue (nodeMintingPolicy config) () burnedNodeToken
    ]
  where
    burnedNodeToken =
      singleton
        (scriptCurrencySymbol (nodeMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
        (-1)
