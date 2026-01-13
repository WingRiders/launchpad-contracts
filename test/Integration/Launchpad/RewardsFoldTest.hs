{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Integration.Launchpad.RewardsFoldTest (rewardsFoldTests) where

import Control.Monad (forM_, when)
import Data.Default (def)
import Data.List (sort)
import Integration.Launchpad.CommitFold
import Integration.Launchpad.Node
import Integration.Launchpad.Options (RunLimitTests (..), ThoroughTests (..))
import Integration.Launchpad.ProjectTokensHolderFirst
import Integration.Launchpad.RewardsFold
import Integration.Mock
import Integration.Util
import Launchpad.Node qualified as N
import Launchpad.RewardsFold qualified as R
import Launchpad.Types
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValue)
import Test.Tasty
import Test.Util (vUSDT)

rewardsFoldTests :: TestTree
rewardsFoldTests = askOption \(ThoroughTests thorough) ->
  testGroup "rewardsFold" $
    [ rewardsFoldMintingPolicyTests adaAssetClass
    , rewardsFoldApplicationTests adaAssetClass
    , rewardsFoldLimitsTests
    ]
      <> case thorough of
        False -> []
        True ->
          [ rewardsFoldMintingPolicyTests vUSDT
          , rewardsFoldApplicationTests vUSDT
          ]

rewardsFoldLimitsTests :: TestTree
rewardsFoldLimitsTests = askOption $ \case
  RunLimitTests True ->
    testGroup
      "Rewards fold limits"
      [ good
          defaultLaunchpadConfig
          "The rewards fold can be applied to 16 default nodes"
          (applyRewardsNodesCutoffTest 0 16)
      , good
          defaultLaunchpadConfig
          "The rewards fold can be applied to 4 presale nodes and 12 default nodes"
          (applyRewardsNodesCutoffTest 4 12)
      , good
          defaultLaunchpadConfig
          "The rewards fold can be applied to 16 presale nodes"
          (applyRewardsNodesCutoffTest 16 0)
      , bad
          defaultLaunchpadConfig
          "The rewards fold cannot be applied to 17 default nodes"
          (applyRewardsNodesCutoffTest 0 17)
      , bad
          defaultLaunchpadConfig
          "The rewards fold cannot be applied to 5 presale nodes and 12 default nodes"
          (applyRewardsNodesCutoffTest 5 12)
      , bad
          defaultLaunchpadConfig
          "The rewards fold cannot be applied to 17 presale nodes"
          (applyRewardsNodesCutoffTest 17 0)
      ]
  RunLimitTests False -> testGroup "rewardsFoldLimits" []

rewardsFoldMintingPolicyTests :: AssetClass -> TestTree
rewardsFoldMintingPolicyTests commitmentAsset = askOption \(ThoroughTests thorough) ->
  testGroup
    ("rewardsFoldMintingPolicy - committed " <> if commitmentAsset == adaAssetClass then "ADA" else show commitmentAsset)
    $ [ good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold can be created"
          createRewardsFoldTest
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold cannot be created when the node is not the head node"
          failsInitRewardsFoldNotHeadNode
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold cannot be created when the commit fold is not finished"
          failsInitRewardsFoldNotFinishedCommitFold
      ]
      <> case thorough of
        False -> []
        True ->
          [ bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created if the node doesn't have the token"
              failsInitRewardsFoldNoNodeToken
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created if the commit fold doesn't have the token"
              failsInitRewardsFoldNoCommitToken
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created if the rewards fold token was not minted"
              failsInitRewardsFoldNoRewardsToken
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the commit fold has a different node script hash"
              failsInitRewardsFoldDifferentCommitNodeScriptHash
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the rewards fold has a different node script hash"
              failsInitRewardsFoldDifferentRewardsNodeScriptHash
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the rewards fold has a different committed value"
              failsInitRewardsFoldDifferentCommittedValue
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the rewards fold has a cutoff key and the commit fold doesn't"
              failsInitRewardsFoldWithCutoffKey
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the rewards fold has a cutoff time and the commit fold doesn't"
              failsInitRewardsFoldWithCutoffTime
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the commit fold has a cutoff key and the rewards fold doesn't"
              failsInitRewardsFoldNoCutoffKey
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the rewards fold has a different overcommitted value"
              failsInitRewardsFoldDifferentOvercommittedValue
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold cannot be created when the rewards fold has a different commit fold owner"
              failsInitRewardsFoldDifferentOwner
          ]

rewardsFoldApplicationTests :: AssetClass -> TestTree
rewardsFoldApplicationTests commitmentAsset = askOption \(ThoroughTests thorough) ->
  testGroup
    ("rewardsFoldApplication - committed " <> if commitmentAsset == adaAssetClass then "ADA" else show commitmentAsset)
    $ [ good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold can be applied to one middle node without the cutoff"
          (foldOverNodes' [FourDefault] Nothing)
      , good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold can be applied to an ineligible node with the cutoff"
          (foldOverNodes' [OnePresale] (Just (OnePresale, 10)))
      , good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold can be applied to the cutoff node"
          (foldOverNodes' [FourDefault] (Just (FourDefault, 10)))
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The rewards fold can't pay less rewards"
          failsApplyRewardsFoldLessRewards
      ]
      <> case thorough of
        False -> []
        True ->
          [ good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to one middle presale tier node without the cutoff"
              (foldOverNodes' [OnePresale] Nothing)
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to one last node without the cutoff"
              (foldOverNodes' [LastDefault] Nothing)
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to one last presale tier node without the cutoff"
              (foldOverNodes' [LastPresale] Nothing)
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to a middle node and the last node without the cutoff"
              (foldOverNodes' [FourDefault, LastDefault] Nothing)
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to two middle nodes without the cutoff"
              (foldOverNodes' [ThreePresale, FourDefault] Nothing)
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to an eligible node with the cutoff"
              (foldOverNodes' [FourDefault] (Just (LastDefault, 10)))
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to an eligible presale tier node with the cutoff"
              (foldOverNodes' [OnePresale] (Just (LastDefault, 10)))
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to an ineligible presale tier node with the cutoff"
              (foldOverNodes' [LastPresale] (Just (OnePresale, 10)))
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to the cutoff presale tier node"
              (foldOverNodes' [OnePresale] (Just (OnePresale, 10)))
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to the cutoff node with 0 overcommitment"
              (foldOverNodes' [OnePresale] (Just (OnePresale, 0)))
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can be applied to the cutoff presale tier node with 0 overcommitment"
              (foldOverNodes' [LastPresale] (Just (LastPresale, 0)))
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold can't be applied without the rewards token"
              failsApplyRewardsFoldNoRewardsToken
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold's last step can't omit paying to the commit fold owner"
              failsApplyRewardsFoldOmitCommitFoldOwnerCompensation
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold fails if the node tokens are not burned"
              failsApplyRewardsNoBurning
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold fails if the rewards holder staking credential does not match node"
              failsRewardsHolderStakingCredentialMismatch
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold fails if the project tokens holder final staking credential does not match first"
              failsTokensHolderFinalStakingCredentialMismatch
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The rewards fold fails if the project tokens holder first staking credential changed"
              failsTokensHolderFirstStakingCredentialMismatch
          ]

failsApplyRewardsFoldOmitCommitFoldOwnerCompensation :: LaunchpadConfig -> Run ()
failsApplyRewardsFoldOmitCommitFoldOwnerCompensation config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          Nothing
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
      node = Node (Just nodeKey) Nothing 0 100
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin node AddToken
  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver config admin def {nodeKeys = [Just nodeKey], iteration = LastNode, omitCommitFoldCompensation = True}

failsApplyRewardsFoldLessRewards :: LaunchpadConfig -> Run ()
failsApplyRewardsFoldLessRewards config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          Nothing
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
      node = Node (Just nodeKey) Nothing 0 100
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin node AddToken
  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver
    config
    admin
    def
      { nodeKeys = [Just nodeKey]
      , iteration = LastNode
      , stealedRewards = (0, assetClassValue config.projectToken 1)
      }

failsApplyRewardsFoldNoRewardsToken :: LaunchpadConfig -> Run ()
failsApplyRewardsFoldNoRewardsToken config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          Nothing
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
      node = Node (Just nodeKey) Nothing 0 100
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin node AddToken
  createRewardsFold config admin 2 rewardsFold SkipToken
  rewardsFoldOver config admin def {nodeKeys = [Just nodeKey], iteration = LastNode}

failsApplyRewardsNoBurning :: LaunchpadConfig -> Run ()
failsApplyRewardsNoBurning config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      node = Node (Just nodeKey) Nothing 0 100
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          (Just nodeKey)
          (Just 0)
          1_000
          10
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin node AddToken
  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver config admin def {burnNodeTokens = False, nodeKeys = [Just nodeKey], iteration = LastNode}

applyRewardsNodesCutoffTest :: Int -> Int -> LaunchpadConfig -> Run ()
applyRewardsNodesCutoffTest tieredNodesCount defaultNodesCount config = do
  _ <- setupWallets config
  admin <- getMainUser

  let nodesCount = tieredNodesCount + defaultNodesCount

  users <- sequence (take nodesCount (repeat (newUser defaultTokens)))
  let sortedKeys = map (\k -> (unwrapPubKeyHash k, 0)) (sort users)
      nodes =
        zipWith3
          ( \key nextKey index ->
              Node
                (Just key)
                (Just nextKey)
                -- Half the nodes are after the cutoff
                (if index > nodesCount `div` 2 then 0 else 2)
                100
          )
          sortedKeys
          (tail sortedKeys)
          [0 ..]
          <> [Node (Just (last sortedKeys)) Nothing 2 100]
      tieredNodes = take tieredNodesCount nodes
      defaultNodes = drop tieredNodesCount nodes
      createPresaleNode node = createTierNode config admin node Presale AddToken
      createDefaultNode node = createTierNode config admin node Default AddToken
      rewardsFold =
        RewardsFoldDatum
          { nodeScriptHash = N.nodeScriptValidatorHash (nodeConfig config)
          , next = Just (head sortedKeys)
          , cutoffKey = Just (head sortedKeys)
          , cutoffTime = Just 1
          , committed = 1_000
          , overcommitted = 0
          , commitFoldOwner = pubKeyHashAddress admin
          }
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  forM_ tieredNodes createPresaleNode
  forM_ defaultNodes createDefaultNode
  createRewardsFold config admin 2 rewardsFold AddToken

  rewardsFoldOver config admin def {nodeKeys = Just <$> sortedKeys, iteration = LastNode}

failsRewardsHolderStakingCredentialMismatch :: LaunchpadConfig -> Run ()
failsRewardsHolderStakingCredentialMismatch config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      node = Node (Just nodeKey) Nothing 0 100
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          (Just nodeKey)
          (Just 0)
          1_000
          10
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin node AddToken
  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver config admin def {nodeKeys = [Just nodeKey], iteration = LastNode, differentStakingCredentialReward = True}

failsTokensHolderFinalStakingCredentialMismatch :: LaunchpadConfig -> Run ()
failsTokensHolderFinalStakingCredentialMismatch config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      node = Node (Just nodeKey) Nothing 0 100
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          (Just nodeKey)
          (Just 0)
          1_000
          10
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin node AddToken
  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver config admin def {nodeKeys = [Just nodeKey], iteration = LastNode, differentStakingCredentialHolder = True}

failsTokensHolderFirstStakingCredentialMismatch :: LaunchpadConfig -> Run ()
failsTokensHolderFirstStakingCredentialMismatch config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      nodeKey3 = (unwrapPubKeyHash userWallet3, 0)
      (sortedKey1, sortedKey2, sortedKey3) = case sort [nodeKey1, nodeKey2, nodeKey3] of
        [a, b, c] -> (a, b, c)
        _ -> error "Impossible"
      middleNode1 = Node (Just sortedKey1) (Just sortedKey2) 0 100
      middleNode2 = Node (Just sortedKey2) (Just sortedKey3) 0 100
      rewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just sortedKey1)
          Nothing
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken
  createNode config admin middleNode1 AddToken
  createNode config admin middleNode2 AddToken
  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver config admin def {nodeKeys = [Just sortedKey1, Just sortedKey2], iteration = NotLastNode, differentStakingCredentialHolder = True}

data NodeNumber
  = OnePresale
  | TwoPresale
  | ThreePresale
  | FourDefault
  | LastDefault
  | LastPresale
  deriving (Eq)
type CutoffNode = Maybe (NodeNumber, Integer)

foldOverNodes' :: [NodeNumber] -> CutoffNode -> LaunchpadConfig -> Run ()
foldOverNodes' foldedNodes cutoffNode config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser

  let (sortedPkh1, sortedPkh2, sortedPkh3) = case sort [(userWallet1), (userWallet2), (userWallet3)] of
        [a, b, c] -> (a, b, c)
        _ -> error "sort failed"

      node1 = Node (Just (unwrapPubKeyHash sortedPkh1, 0)) node2.key 0 100
      node2 = Node (Just (unwrapPubKeyHash sortedPkh2, 0)) node3.key 1 100
      node3 = Node (Just (unwrapPubKeyHash sortedPkh3, 0)) node4.key 2 100
      node4 = Node (Just (unwrapPubKeyHash sortedPkh3, 1)) node5.key 3 100
      node5 = Node (Just (unwrapPubKeyHash sortedPkh3, 2)) Nothing 4 100

      nodeOf OnePresale = node1
      nodeOf TwoPresale = node2
      nodeOf ThreePresale = node3
      nodeOf FourDefault = node4
      nodeOf LastDefault = node5
      nodeOf LastPresale = node5

      foldedKeys = map (key . nodeOf) foldedNodes
      cutoffKey = key . nodeOf . fst =<< cutoffNode
      cutoffTime = createdTime . nodeOf . fst <$> cutoffNode
      overcommitted = maybe 0 snd cutoffNode

      iteration =
        if (elem LastPresale foldedNodes || elem LastDefault foldedNodes)
          then LastNode
          else NotLastNode

      rewardsFold =
        RewardsFoldDatum
          { nodeScriptHash = N.nodeScriptValidatorHash (nodeConfig config)
          , next = head foldedKeys
          , cutoffKey
          , cutoffTime
          , committed = 1_000
          , overcommitted
          , commitFoldOwner = pubKeyHashAddress admin
          }

  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createRewardsFoldRefScript config admin
  createFirstProjectTokensHolder config admin 2 AddToken

  when (elem LastDefault foldedNodes && elem LastPresale foldedNodes) $
    error "can't create the same node with different tiers"

  when (elem OnePresale foldedNodes) $
    createTierNode config admin node1 Presale AddToken
  when (elem TwoPresale foldedNodes) $
    createTierNode config admin node2 Presale AddToken
  when (elem ThreePresale foldedNodes) $
    createTierNode config admin node3 Presale AddToken
  when (elem FourDefault foldedNodes) $
    createTierNode config admin node4 Default AddToken
  when (elem LastDefault foldedNodes) $
    createTierNode config admin node5 Default AddToken
  when (elem LastPresale foldedNodes) $
    createTierNode config admin node5 Presale AddToken

  createRewardsFold config admin 2 rewardsFold AddToken
  rewardsFoldOver config admin def {nodeKeys = foldedKeys, iteration}

failsInitRewardsFoldDifferentOwner :: LaunchpadConfig -> Run ()
failsInitRewardsFoldDifferentOwner config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          (Just nodeKey)
          Nothing
          1_000
          0
          (pubKeyHashAddress userWallet2)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldDifferentOvercommittedValue :: LaunchpadConfig -> Run ()
failsInitRewardsFoldDifferentOvercommittedValue config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          (Just nodeKey)
          Nothing
          1_000
          1
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldWithCutoffTime :: LaunchpadConfig -> Run ()
failsInitRewardsFoldWithCutoffTime config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          Nothing
          (Just 1)
          1_000
          0
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldNoCutoffKey :: LaunchpadConfig -> Run ()
failsInitRewardsFoldNoCutoffKey config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          (Just nodeKey)
          (Just 1)
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          Nothing
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldWithCutoffKey :: LaunchpadConfig -> Run ()
failsInitRewardsFoldWithCutoffKey config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          (Just nodeKey)
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldDifferentCommittedValue :: LaunchpadConfig -> Run ()
failsInitRewardsFoldDifferentCommittedValue config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          Nothing
          Nothing
          1_001
          0
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldDifferentRewardsNodeScriptHash :: LaunchpadConfig -> Run ()
failsInitRewardsFoldDifferentRewardsNodeScriptHash config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)
      wrongRewardsFold =
        RewardsFoldDatum
          (R.rewardsFoldScriptValidatorHash (rewardsFoldConfig config))
          (Just nodeKey)
          Nothing
          Nothing
          1_000
          0
          (pubKeyHashAddress admin)
  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin (Just wrongRewardsFold) Nothing AddToken

failsInitRewardsFoldDifferentCommitNodeScriptHash :: LaunchpadConfig -> Run ()
failsInitRewardsFoldDifferentCommitNodeScriptHash config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (R.rewardsFoldScriptValidatorHash (rewardsFoldConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin Nothing Nothing AddToken

failsInitRewardsFoldNotFinishedCommitFold :: LaunchpadConfig -> Run ()
failsInitRewardsFoldNotFinishedCommitFold config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      unfinishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          (Just nodeKey)
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin unfinishedCommitFold AddToken
  initRewardsFold config admin Nothing Nothing AddToken

failsInitRewardsFoldNotHeadNode :: LaunchpadConfig -> Run ()
failsInitRewardsFoldNotHeadNode config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      node = Node (Just nodeKey) Nothing 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin node AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin Nothing (Just nodeKey) AddToken

failsInitRewardsFoldNoRewardsToken :: LaunchpadConfig -> Run ()
failsInitRewardsFoldNoRewardsToken config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin Nothing Nothing SkipToken

failsInitRewardsFoldNoCommitToken :: LaunchpadConfig -> Run ()
failsInitRewardsFoldNoCommitToken config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold SkipToken
  initRewardsFold config admin Nothing Nothing AddToken

failsInitRewardsFoldNoNodeToken :: LaunchpadConfig -> Run ()
failsInitRewardsFoldNoNodeToken config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode SkipToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin Nothing Nothing AddToken

createRewardsFoldTest :: LaunchpadConfig -> Run ()
createRewardsFoldTest config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
      headNode = Node Nothing (Just nodeKey) 0 0
      finishedCommitFold =
        CommitFoldDatum
          (N.nodeScriptValidatorHash (nodeConfig config))
          Nothing
          1_000
          Nothing
          Nothing
          0
          2
          (pubKeyHashAddress admin)

  createNodeRefScript config admin
  createCommitFoldRefScript config admin
  createNode config admin headNode AddToken
  createCommitFold config admin finishedCommitFold AddToken
  initRewardsFold config admin Nothing Nothing AddToken
