{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Integration.Launchpad.RewardsFold where

import Cardano.Simple.Ledger.Tx qualified as Tx
import Data.Default qualified as D
import Data.List (find, sortOn)
import Data.Maybe
import Data.Set qualified as Set
import Integration.Launchpad.CommitFold
import Integration.Launchpad.Node
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util
import Launchpad.Mint.ProjectTokensHolder qualified as PTH
import Launchpad.Mint.RewardsFold (rewardsFoldMintingPolicySymbol)
import Launchpad.Types
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutus.Model
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue, valueOf)
import PlutusLedgerApi.V1.Value qualified as V
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (Datum (..), FromData (..), OutputDatum (..), PubKeyHash (..), ToData (..), TxOut (..), TxOutRef, Value, singleton)
import PlutusTx.Prelude (inv)

data RewardsFoldApplicationConfig = RewardsFoldApplicationConfig
  { newRewardsDatum :: Maybe RewardsFoldDatum
  , nodeKeys :: [Maybe NodeKey]
  , iteration :: RewardsFoldIteration
  , omitCommitFoldCompensation :: Bool
  , burnNodeTokens :: Bool
  , differentStakingCredentialReward :: Bool
  , differentStakingCredentialHolder :: Bool
  , stealedRewards :: (Integer, Value)
  }

instance D.Default RewardsFoldApplicationConfig where
  def =
    RewardsFoldApplicationConfig
      { newRewardsDatum = Nothing
      , nodeKeys = []
      , iteration = NotLastNode
      , omitCommitFoldCompensation = False
      , burnNodeTokens = True
      , differentStakingCredentialReward = False
      , differentStakingCredentialHolder = False
      , stealedRewards = (0, mempty)
      }

createRewardsFoldRefScript :: LaunchpadConfig -> PubKeyHash -> Run ()
createRewardsFoldRefScript config wallet = do
  usp <- spend wallet (adaValue 1)
  ensureTx wallet (loadRefScript (rewardsFoldValidator config) (adaValue 1) <> userSpend usp)

initRewardsFold :: LaunchpadConfig -> PubKeyHash -> Maybe RewardsFoldDatum -> Maybe NodeKey -> AddToken -> Run ()
initRewardsFold config wallet rewardsFoldDatum nodeKey addToken = do
  nodeUtxos <- utxoAt (nodeValidator config)
  let headNodeUtxo = findNodeUtxo nodeKey nodeUtxos
  [(nodeRefScript, _)] <- refScriptAt (nodeValidator config)
  [commitFoldUtxo] <- filter (isNothing . txOutReferenceScript . snd) <$> utxoAt (commitFoldValidator config)
  [(commitFoldRefScript, _)] <- refScriptAt (commitFoldValidator config)
  range <- currentTimeRad 1_000
  submitTx wallet
    =<< validateIn
      range
      ( initRewardsFoldTx
          config
          wallet
          nodeRefScript
          commitFoldRefScript
          headNodeUtxo
          commitFoldUtxo
          rewardsFoldDatum
          addToken
      )

initRewardsFoldTx :: LaunchpadConfig -> PubKeyHash -> TxOutRef -> TxOutRef -> (TxOutRef, TxOut) -> (TxOutRef, TxOut) -> Maybe RewardsFoldDatum -> AddToken -> Tx
initRewardsFoldTx config wallet nodeRefScript commitFoldRefScript (nodeRef, nodeOut) (commitFoldRef, commitFoldOut) datum addToken =
  mconcat
    [ memptyIf V.isZero rewardsToken (mintValue (rewardsFoldMintingPolicy config) ())
    , memptyIf V.isZero commitToken (mintValue (commitFoldMintingPolicy config) ())
    , memptyIf V.isZero nodeToken (mintValue (nodeMintingPolicy config) ())
    , payToScript (rewardsFoldValidator config) (InlineDatum rewardsFoldDatum) (rewardsToken <> adaValue foldOilAdaAmount)
    , spendScriptRef nodeRefScript (nodeValidator config) nodeRef StartRewardsFold nodeDatum
    , spendScriptRef commitFoldRefScript (commitFoldValidator config) commitFoldRef DelegateCommitToNode commitFoldDatum
    , -- oil for the rewards fold
      -- oil for the rewards holder from the head node
      -- min ada from the commit fold
      payToKey wallet (adaValue (rewardsHolderOilAdaAmount + commitFoldFeeAdaAmount + rewardsFoldFeeAdaAmount))
    ]
  where
    nodeDatum = txOutputDatum nodeOut
    commitFoldDatum = txOutputDatum commitFoldOut
    rewardsFoldDatum = case datum of
      Just d -> d
      Nothing ->
        RewardsFoldDatum
          { nodeScriptHash = toValidatorHash (nodeValidator config)
          , next = nodeDatum.next
          , cutoffKey = commitFoldDatum.cutoffKey
          , cutoffTime = commitFoldDatum.cutoffTime
          , committed = commitFoldDatum.committed
          , overcommitted = commitFoldDatum.overcommitted
          , commitFoldOwner = commitFoldDatum.owner
          }

    rewardsToken = case addToken of
      AddToken ->
        singleton
          (scriptCurrencySymbol (rewardsFoldMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (rewardsFoldValidator config)))
          1
      SkipToken -> mempty

    nodeToken =
      if valueOf
        (txOutValue nodeOut)
        (scriptCurrencySymbol (nodeMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
        == 0
        then mempty
        else
          singleton
            (scriptCurrencySymbol (nodeMintingPolicy config))
            (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
            (-1)

    commitToken =
      if valueOf
        (txOutValue commitFoldOut)
        (scriptCurrencySymbol (commitFoldMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
        == 0
        then mempty
        else
          singleton
            (scriptCurrencySymbol (commitFoldMintingPolicy config))
            (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
            (-1)

createRewardsFold :: LaunchpadConfig -> PubKeyHash -> Integer -> RewardsFoldDatum -> AddToken -> Run ()
createRewardsFold config wallet foldedNodes rewardsFoldDatum addToken = do
  let value =
        adaValue (foldOilAdaAmount * foldedNodes)
          <> case addToken of
            AddToken ->
              singleton
                (scriptCurrencySymbol (rewardsFoldMintingPolicy config))
                (scriptHashToTokenName (toValidatorHash (rewardsFoldValidator config)))
                1
            SkipToken -> mempty
  usp <- spend wallet value
  ensureTx wallet (createRewardsFoldTx config usp rewardsFoldDatum value)

createRewardsFoldTx :: LaunchpadConfig -> UserSpend -> RewardsFoldDatum -> Value -> Tx
createRewardsFoldTx config usp datum value =
  mconcat
    [ userSpend usp
    , payToScript (rewardsFoldValidator config) (InlineDatum datum) value
    ]

data RewardsFoldIteration = LastNode | NotLastNode

rewardsFoldOver :: LaunchpadConfig -> PubKeyHash -> RewardsFoldApplicationConfig -> Run ()
rewardsFoldOver config wallet RewardsFoldApplicationConfig {..} = do
  nodes <- utxoAt (nodeValidator config)
  let keyedNodes =
        filter
          ( \(_, out) -> case txOutDatum out of
              OutputDatum (Datum d) -> elem (key (fromJust (fromBuiltinData (toBuiltinData d)))) nodeKeys
              _ -> False
          )
          nodes
      nodesWithIndices = zipWith (\n i -> (n, txOutputDatum (snd n), i)) keyedNodes [0 ..]

      findNodeOutByRef ref = snd $ fromJust $ find (\(ref', _) -> ref == ref') nodes

  [(nodeRefScript, _)] <- refScriptAt (nodeValidator config)
  [(rewardsFoldRefScript, _)] <- refScriptAt (rewardsFoldValidator config)
  [(firstTokensHolderRefScript, _)] <- refScriptAt (projectTokensHolderFirstValidator config)

  [(rewardsFoldRef, rewardsFoldOut)] <- filter (isNothing . txOutReferenceScript . snd) <$> utxoAt (rewardsFoldValidator config)
  [(firstTokensHolderRef, firstTokensHolderOut)] <- filter (isNothing . txOutReferenceScript . snd) <$> utxoAt (projectTokensHolderFirstValidator config)

  let tx =
        rewardsFoldOverTx
          config
          iteration
          omitCommitFoldCompensation
          burnNodeTokens
          differentStakingCredentialReward
          differentStakingCredentialHolder
          (nodeRefScript, rewardsFoldRefScript, firstTokensHolderRefScript)
          (rewardsFoldRef, rewardsFoldOut, 0)
          (firstTokensHolderRef, firstTokensHolderOut, 0)
          newRewardsDatum
          nodesWithIndices
          stealedRewards
          wallet
      nodeKey i = case i.txInType of
        Just (Tx.ConsumeScriptAddress _ _ d) -> key (fromJust (fromBuiltinData (toBuiltinData d)))
        _ -> Nothing

      inputs = zip (Set.toList tx.tx'plutus.txInputs) [0 ..]

      -- isValidatorInput doesn't work with ref scripts, so we need to check the datum type
      rewardsFoldIndex = tryDatumTypeIndex @RewardsFoldDatum inputs
      tokensHolderIndex = tryDatumTypeIndex @LaunchpadTokensHolderDatum inputs
      nodeInputs = filter ((hasDatumType @Node) . fst) inputs

      sortedNodeInputs = sortOn (nodeKey . fst) nodeInputs
      correctNodes =
        map
          ( \(i, index) -> case i.txInType of
              Just (Tx.ConsumeScriptAddress _ _ d) ->
                ((i.txInRef, findNodeOutByRef i.txInRef), fromJust (fromBuiltinData @Node (toBuiltinData d)), index)
              _ -> error "impossible"
          )
          sortedNodeInputs

  range <- currentTimeRad 1_000
  realTx <-
    validateIn
      range
      ( rewardsFoldOverTx
          config
          iteration
          omitCommitFoldCompensation
          burnNodeTokens
          differentStakingCredentialReward
          differentStakingCredentialHolder
          (nodeRefScript, rewardsFoldRefScript, firstTokensHolderRefScript)
          (rewardsFoldRef, rewardsFoldOut, rewardsFoldIndex)
          (firstTokensHolderRef, firstTokensHolderOut, tokensHolderIndex)
          newRewardsDatum
          correctNodes
          stealedRewards
          wallet
      )

  submitTx wallet realTx

nodeReward :: LaunchpadConfig -> Bool -> RewardsFoldDatum -> (TxOut, Node) -> Value -> Maybe (Tx, Integer)
nodeReward config differentStakingCredentialReward rewardsFold (nodeOut, node) addedValue = if separatorNode || headNode then Nothing else Just (rewardsOutput, nodeRewards)
  where
    rewardsOutput =
      if differentStakingCredentialReward
        then
          payToScript
            (appendStakingCredential mockStakingCredential (rewardsHolderValidator config))
            (InlineDatum (RewardsHolderDatum nodeKey projectSymbol projectToken raisingSymbol raisingToken))
            rewardsHolderValue
        else
          payToScript
            (rewardsHolderValidator config)
            (InlineDatum (RewardsHolderDatum nodeKey projectSymbol projectToken raisingSymbol raisingToken))
            rewardsHolderValue

    rewardsHolderValue =
      adaValue rewardsHolderOilAdaAmount
        <> singleton raisingSymbol raisingToken (node.committed - nodeCommit)
        <> singleton projectSymbol projectToken nodeRewards
        <> addedValue
        <> presentTierTokens
    nodeRewards = div (config.tokensToDistribute * nodeCommit) rewardsFold.committed
    presentTierTokens = Value.unionWith min nodeOut.txOutValue $ tierToken config Presale
    nodeCommit = nodeCommitment rewardsFold.cutoffTime rewardsFold.cutoffKey rewardsFold.overcommitted node
    separatorNode = node.committed == 0
    headNode = node.key == Nothing
    nodeKey = fromJust node.key
    AssetClass (projectSymbol, projectToken) = config.projectToken
    AssetClass (raisingSymbol, raisingToken) = config.raisingToken

rewardsFoldOverTx ::
  LaunchpadConfig ->
  RewardsFoldIteration ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  (TxOutRef, TxOutRef, TxOutRef) ->
  (TxOutRef, TxOut, Integer) ->
  (TxOutRef, TxOut, Integer) ->
  Maybe RewardsFoldDatum ->
  [((TxOutRef, TxOut), Node, Integer)] ->
  (Integer, Value) ->
  PubKeyHash ->
  Tx
rewardsFoldOverTx
  config
  iteration
  omitCommitFoldCompensation
  burnNodeTokens
  differentStakingCredentialReward
  differentStakingCredentialHolder
  (nodeRefScript, rewardsFoldRefScript, firstTokensHolderRefScript)
  (rewardsFoldRef, rewardsFoldOut, rewardsFoldIndex)
  (firstTokensHolderRef, firstTokensHolderOut, tokensHolderIndex)
  resultingDatum
  nodeUtxos
  (stealedIndex, stealedRewards)
  wallet =
    mconcat $
      map
        (\((ref, _), node, _) -> spendScriptRef nodeRefScript (nodeValidator config) ref (DelegateToRewardsFold rewardsFoldIndex) node)
        nodeUtxos
        <> [ spendScriptRef
              rewardsFoldRefScript
              (rewardsFoldValidator config)
              rewardsFoldRef
              ( RewardsFold
                  (map (\(_, _, i) -> i) nodeUtxos)
                  -- shifted by 3:
                  -- 0th index is the rewards fold or the commit fold compensation
                  -- 1st index is the tokens holder
                  -- 2nd index is the rewards fold owner claiming the fold fee
                  (map (+ 3) rewardIndices)
                  0
                  rewardsFoldIndex
                  tokensHolderIndex
                  0 -- TODO: fix
                  0 -- TODO: fix
              )
              oldRewardsFoldDatum
           , spendScriptRef
              firstTokensHolderRefScript
              (projectTokensHolderFirstValidator config)
              firstTokensHolderRef
              DelegateToRewardsOrFailure
              (txOutputDatum firstTokensHolderOut)
           , case iteration of
              NotLastNode ->
                payToScript
                  (rewardsFoldValidator config)
                  (InlineDatum newRewardsFoldDatum)
                  (adaValue (commitFoldFeeAdaAmount * fromIntegral (length nodeUtxos)) <> txOutValue rewardsFoldOut)
              LastNode ->
                ( payToKey
                    (if omitCommitFoldCompensation then config.daoFeeReceiver else commitFoldOwner)
                    ( adaValue (commitFoldFeeAdaAmount * fromIntegral (length nodeUtxos))
                        <> ada (adaOf (txOutValue rewardsFoldOut))
                    )
                )
                  <> mintValue
                    (rewardsFoldMintingPolicy config)
                    ()
                    ( singleton
                        (rewardsFoldMintingPolicySymbol (rewardsFoldPolicyConfig config))
                        (scriptHashToTokenName (toValidatorHash (rewardsFoldValidator config)))
                        (-1)
                    )
                  <> mintValue
                    (projectTokensHolderMintingPolicy config)
                    ()
                    ( singleton
                        (PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig config))
                        (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config)))
                        (-1)
                    )
           , case iteration of
              NotLastNode ->
                if differentStakingCredentialHolder
                  then
                    payToScript
                      (appendStakingCredential mockStakingCredential (projectTokensHolderFirstValidator config))
                      (InlineDatum (txOutputDatum firstTokensHolderOut))
                      ( txOutValue firstTokensHolderOut
                          <> assetClassValue config.raisingToken collectedCommitted
                          <> memptyIfZero (singleton projectSymbol projectToken (-distributedRewards))
                      )
                  else
                    payToScript
                      (projectTokensHolderFirstValidator config)
                      (InlineDatum (txOutputDatum firstTokensHolderOut))
                      ( txOutValue firstTokensHolderOut
                          <> assetClassValue config.raisingToken collectedCommitted
                          <> memptyIfZero (singleton projectSymbol projectToken (-distributedRewards))
                      )
              LastNode ->
                if differentStakingCredentialHolder
                  then
                    payToScript
                      (appendStakingCredential mockStakingCredential (projectTokensHolderFinalValidator config))
                      (InlineDatum Wr)
                      ( txOutValue firstTokensHolderOut
                          <> assetClassValue config.raisingToken collectedCommitted
                          <> memptyIfZero (singleton projectSymbol projectToken (-distributedRewards))
                          <> singleton
                            (PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig config))
                            (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config)))
                            (-1)
                      )
                  else
                    payToScript
                      (projectTokensHolderFinalValidator config)
                      (InlineDatum Wr)
                      ( txOutValue firstTokensHolderOut
                          <> assetClassValue config.raisingToken collectedCommitted
                          <> memptyIfZero (singleton projectSymbol projectToken (-distributedRewards))
                          <> ( singleton
                                (PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig config))
                                (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config)))
                                (-1)
                             )
                      )
           , -- that is the rewards fold fee
             payToKey
              wallet
              ( adaValue (rewardsFoldFeeAdaAmount * fromIntegral (length nodeUtxos))
                  -- leftover ADA from separator nodes
                  <> adaValue (rewardsHolderOilAdaAmount * fromIntegral (separatorNodes))
                  <> stealedRewards
                  <> if burnNodeTokens then mempty else (inv burnedNodeTokens)
              )
           , if burnNodeTokens then memptyIf V.isZero burnedNodeTokens (mintValue (nodeMintingPolicy config) ()) else mempty
           ]
        <> map (maybe mempty fst) rewards
    where
      commitFoldOwner = oldRewardsFoldDatum.commitFoldOwner

      AssetClass (projectSymbol, projectToken) = config.projectToken

      distributedRewards = sum (map (maybe 0 snd) rewards)
      rewards =
        zipWith
          ( \((_, out), node, _) i ->
              nodeReward config differentStakingCredentialReward oldRewardsFoldDatum (out, node) (if i == stealedIndex then stealedRewards else mempty)
          )
          nodeUtxos
          [0 ..]
      rewardIndices = calculateRewardIndices rewards
      separatorNodes = length $ filter (== (-4)) rewardIndices
      burnedNodeTokens =
        mconcat $
          map
            ( const
                ( singleton
                    (scriptCurrencySymbol (nodeMintingPolicy config))
                    (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
                    (-1)
                )
            )
            nodeUtxos
      collectedCommitted =
        countCommitted
          oldRewardsFoldDatum.cutoffTime
          oldRewardsFoldDatum.cutoffKey
          oldRewardsFoldDatum.overcommitted
          (map (\(_, node, _) -> node) nodeUtxos)
      lastNode = (\(_, node, _) -> node) . last . sortOn (\(_, node, _) -> node) $ nodeUtxos
      oldRewardsFoldDatum = txOutputDatum rewardsFoldOut
      newRewardsFoldDatum = case resultingDatum of
        Just d -> d
        Nothing -> (oldRewardsFoldDatum :: RewardsFoldDatum) {next = lastNode.next}

calculateRewardIndices :: [Maybe (Tx, Integer)] -> [Integer]
calculateRewardIndices rewards = snd $ foldl go (0, []) rewards
  where
    go (i, acc) r = case r of
      Nothing -> (i, acc ++ [-4])
      Just _ -> (i + 1, acc ++ [i])
