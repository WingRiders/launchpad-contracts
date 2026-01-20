{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Integration.Launchpad.NodeTest (nodeTests) where

import Control.Monad (when)
import Data.Default (def)
import Data.Default qualified as D
import Data.List (sort)
import Data.Maybe
import Integration.Launchpad.FailProof hiding (None)
import Integration.Launchpad.Node
import Integration.Launchpad.Options (RunLimitTests (..), ThoroughTests (..))
import Integration.Launchpad.ProjectTokensHolderFirst
import Integration.Mock
import Integration.Util
import Launchpad.Types
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (PubKeyHash (..))
import Test.Tasty
import Test.Util (vETH)

data NodeTestConfig = NodeTestConfig
  { firstNodeChoice :: NodeChoice
  , userChoice :: UserChoice
  , tier :: Tier
  , separatorNodeCount :: Integer
  , maliciousAction :: MaliciousNodeAction
  , timing :: Maybe NodeActionTiming
  }

instance D.Default NodeTestConfig where
  def =
    NodeTestConfig
      { firstNodeChoice = LastNode
      , userChoice = OtherUser
      , tier = Default
      , separatorNodeCount = 1
      , maliciousAction = None
      , timing = Nothing
      }

data NodeChoice
  = LastNode
  | MiddleNode
  | SingleHeadNode
  | HeadNode
data UserChoice
  = OtherUser
  | SameUser

nodeTests :: TestTree
nodeTests = askOption \(ThoroughTests thorough) ->
  testGroup "Node" $
    [ nodeInsertionTests adaAssetClass
    , nodeSeparatorInsertionTests
    , nodeSeparatorLimitsTests
    , nodeRemovalTests adaAssetClass
    , nodeReclamationTests adaAssetClass
    ]
      <> case thorough of
        False -> []
        True ->
          [ nodeIndexTests
          , nodeInsertionTests vETH
          , nodeRemovalTests vETH
          , nodeReclamationTests vETH
          ]

nodeInsertionTests :: AssetClass -> TestTree
nodeInsertionTests commitmentAsset = askOption \(ThoroughTests thorough) ->
  testGroup
    ("Node insertion - committed " <> if commitmentAsset == adaAssetClass then "ADA" else show commitmentAsset)
    $ [ good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node can be inserted onto a proper last node of another user"
          (nodeInsertionTest def)
      , good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node can be inserted onto a proper last node of the same user"
          (nodeInsertionTest def {userChoice = SameUser})
      , good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node can be inserted onto the head node"
          (nodeInsertionTest def {firstNodeChoice = HeadNode})
      , good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node of the presale tier can be inserted onto the last node"
          (nodeInsertionTest def {tier = Presale})
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node cannot be inserted onto the last node with a smaller key"
          (nodeInsertionTest def {maliciousAction = SmallerKey})
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node cannot be inserted without minting the node token"
          (nodeInsertionTest def {maliciousAction = NoMinting})
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "A node cannot be inserted after the contribution phase"
          (nodeInsertionTest def {timing = Just AfterContributionPhase})
      ]
      <> case thorough of
        False -> []
        True ->
          [ good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The node can be inserted onto a proper middle node of another user"
              (nodeInsertionTest def {firstNodeChoice = MiddleNode})
          , good
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The node can be inserted onto a proper middle node of the same user"
              (nodeInsertionTest def {firstNodeChoice = MiddleNode, userChoice = SameUser})
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The node cannot be inserted onto the head node without the proper project tokens holder ref input"
              (nodeInsertionTest def {firstNodeChoice = HeadNode, maliciousAction = NoTokensHolderRef})
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "A user can't mint more tokens to steal them"
              (nodeInsertionTest def {maliciousAction = DoubleMinting})
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The node staking credential can't be modified when inserting another node"
              (nodeInsertionTest def {maliciousAction = ChangeStakingCredential})
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "A node cannot be inserted after the contribution phase"
              (nodeInsertionTest def {timing = Just AfterContributionPhase})
          ]

nodeSeparatorInsertionTests :: TestTree
nodeSeparatorInsertionTests =
  testGroup
    "Node separator insertion"
    [ good
        defaultLaunchpadConfig
        "One node separator can be inserted onto the head node when signed by the launchpad owner"
        (nodeSeparatorInsertionTest def {firstNodeChoice = HeadNode})
    , good
        defaultLaunchpadConfig
        "One node separator can be inserted onto a middle node when signed by the launchpad owner"
        (nodeSeparatorInsertionTest def {firstNodeChoice = MiddleNode})
    , good
        defaultLaunchpadConfig
        "Two node separators can be inserted onto the node when signed by the launchpad owner"
        (nodeSeparatorInsertionTest def {separatorNodeCount = 2})
    , bad
        defaultLaunchpadConfig
        "Node separators can't be inserted when the transaction is not signed by the launchpad owner or the dao"
        (nodeSeparatorInsertionTest def {maliciousAction = WrongSignature})
    ]

nodeSeparatorLimitsTests :: TestTree
nodeSeparatorLimitsTests = askOption \case
  RunLimitTests True ->
    testGroup
      "Node separator limits"
      [ good
          defaultLaunchpadConfig
          "We can insert 63 node separators"
          (nodeSeparatorInsertionTest def {separatorNodeCount = 63})
      , bad
          defaultLaunchpadConfig
          "We cannot insert 64 node separators"
          (nodeSeparatorInsertionTest def {separatorNodeCount = 64})
      ]
  RunLimitTests False -> testGroup "Node separator limits" []

nodeRemovalTests :: AssetClass -> TestTree
nodeRemovalTests commitmentAsset = askOption \(ThoroughTests thorough) ->
  testGroup
    ("Node removal - committed " <> if commitmentAsset == adaAssetClass then "ADA" else show commitmentAsset)
    $ [ good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The last node can be removed by the user who inserted it"
          (nodeRemovalTest def)
      , good
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The middle node can be removed by the user who inserted it"
          (nodeRemovalTest def {firstNodeChoice = MiddleNode})
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "The node cannot be removed by the user who didn't insert it"
          (nodeRemovalTest def {maliciousAction = WrongSignature})
      , bad
          defaultLaunchpadConfig {raisingToken = commitmentAsset}
          "A node cannot be removed after the contribution phase"
          (nodeRemovalTest def {timing = Just AfterContributionPhase})
      ]
      <> case thorough of
        False -> []
        True ->
          [ bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The node owner can't steal the node token"
              (nodeRemovalTest def {maliciousAction = NoMinting})
          , bad
              defaultLaunchpadConfig {raisingToken = commitmentAsset}
              "The node staking credential can't be modified when removing other node"
              (nodeRemovalTest def {maliciousAction = ChangeStakingCredential})
          ]

nodeReclamationTests :: AssetClass -> TestTree
nodeReclamationTests commitmentAsset =
  testGroup
    ("Node reclamation - committed " <> if commitmentAsset == adaAssetClass then "ADA" else show commitmentAsset)
    [ good
        defaultLaunchpadConfig {raisingToken = commitmentAsset}
        "The user node can be reclaimed by the user who inserted it when supplied with the fail proof"
        (nodeReclamation UserReclaims)
    , good
        defaultLaunchpadConfig {raisingToken = commitmentAsset}
        "The separator node can be reclaimed by the launchpad owner when supplied with the fail proof"
        (nodeReclamation AdminReclaims)
    ]

nodeIndexTests :: TestTree
nodeIndexTests =
  testGroup
    "Node indexes"
    [ good defaultLaunchpadConfig "NodeKey index >= 0 (0) is Valid" (nodeIndexTest def 0)
    , good defaultLaunchpadConfig "NodeKey index >= 0 (1) is Valid" (nodeIndexTest def 1)
    , bad defaultLaunchpadConfig "NodeKey index < 0 (-1) is Invalid" (nodeIndexTest def (-1))
    , good defaultLaunchpadConfig "NodeKey index <= 255 (254) is Valid" (nodeIndexTest def 254)
    , good defaultLaunchpadConfig "NodeKey index <= 255 (255) is Valid" (nodeIndexTest def 255)
    , bad defaultLaunchpadConfig "NodeKey index > 255 (256) is Invalid" (nodeIndexTest def 256)
    , good defaultLaunchpadConfig "Separator NodeKey index >= 0 (0) is Valid" (nodeIndexSeparatorTest 0)
    , good defaultLaunchpadConfig "Separator NodeKey index >= 0 (1) is Valid" (nodeIndexSeparatorTest 1)
    , bad defaultLaunchpadConfig "Separator NodeKey index < 0 (-1) is Invalid" (nodeIndexSeparatorTest (-1))
    , good defaultLaunchpadConfig "Separator NodeKey index >= 0 (254) is Valid" (nodeIndexSeparatorTest 254)
    , good defaultLaunchpadConfig "Separator NodeKey index >= 0 (255) is Valid" (nodeIndexSeparatorTest 255)
    , bad defaultLaunchpadConfig "Separator NodeKey index > 255 (256) is Invalid" (nodeIndexSeparatorTest 256)
    ]

nodeReclamation :: ReclamationScenario -> LaunchpadConfig -> Run ()
nodeReclamation scenario config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = case scenario of
        UserReclaims -> (unwrapPubKeyHash userWallet1, 0)
        AdminReclaims -> (unwrapPubKeyHash userWallet1 <> mconcat (take 21 (repeat "00")), 0)
      node = case scenario of
        UserReclaims -> Node (Just nodeKey) Nothing config.defaultStartTime config.defaultTierMinCommitment
        AdminReclaims -> Node (Just nodeKey) Nothing config.defaultStartTime 0
  createNodeRefScript config admin
  createNode config admin node AddToken
  createFailProof config admin
  reclaimNode config scenario admin nodeKey

nodeRemovalTest :: NodeTestConfig -> LaunchpadConfig -> Run ()
nodeRemovalTest NodeTestConfig {..} config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let (sortedKey1, sortedKey2, sortedKey3) = sortedKeys (userWallet1, userWallet2, userWallet3) userChoice maliciousAction
      removedNode = case firstNodeChoice of
        MiddleNode -> Node (Just sortedKey2) (Just sortedKey3) 0 config.defaultTierMinCommitment
        LastNode -> Node (Just sortedKey2) Nothing 0 config.defaultTierMinCommitment
        SingleHeadNode -> error "nodeRemovalTest: can't remove the single head node"
        HeadNode -> error "nodeRemovalTest: can't remove the head node"
      remainingNode = Node (Just sortedKey1) (Just sortedKey2) 0 config.defaultTierMinCommitment
  createNodeRefScript config admin
  createNode config admin removedNode AddToken
  createNode config admin remainingNode AddToken
  waitUntil (config.startTime + minutes 6)
  when (timing == Just AfterContributionPhase) $ do
    waitUntil config.endTime
    waitNSlots 1 -- so the lower approximation is right as well
  removeNode config admin maliciousAction (remainingNode.key, removedNode.key)

sortedKeys :: (PubKeyHash, PubKeyHash, PubKeyHash) -> UserChoice -> MaliciousNodeAction -> (NodeKey, NodeKey, NodeKey)
sortedKeys (a, b, c) userChoice action = case action of
  SmallerKey -> (k3, k2, k1)
  _ -> (k1, k2, k3)
  where
    (k1, k2, k3) = case userChoice of
      OtherUser -> case sort [(unwrapPubKeyHash a, 0), (unwrapPubKeyHash b, 0), (unwrapPubKeyHash c, 0)] of
        [a', b', c'] -> (a', b', c')
        _ -> error "sortedKeys: sort failed"
      SameUser -> ((unwrapPubKeyHash a, 0), (unwrapPubKeyHash a, 1), (unwrapPubKeyHash a, 2))

nodeSeparatorInsertionTest :: NodeTestConfig -> LaunchpadConfig -> Run ()
nodeSeparatorInsertionTest NodeTestConfig {..} config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let (sortedKey1, _sortedKey2, sortedKey3) = sortedKeys (userWallet1, userWallet2, userWallet3) OtherUser maliciousAction
      nodeKey i = ("a", i)
      node = case firstNodeChoice of
        LastNode -> Node (Just sortedKey1) Nothing config.defaultStartTime config.defaultTierMinCommitment
        MiddleNode -> Node (Just sortedKey1) (Just sortedKey3) config.defaultStartTime config.defaultTierMinCommitment
        SingleHeadNode -> Node Nothing Nothing 0 0
        HeadNode -> Node Nothing (Just sortedKey3) 0 0
      separatorNode i t = Node (Just (nodeKey i)) (Just (nodeKey (i + 1))) t 0
      lastSeparatorNode i t = Node (Just (nodeKey i)) node.next t 0
      insertedNodes = take (fromInteger separatorNodeCount - 1) (map separatorNode [0 ..]) <> [lastSeparatorNode (separatorNodeCount - 1)]
  createFirstProjectTokensHolder config admin 0 AddToken
  createNodeRefScript config admin
  createNode config admin node AddToken
  insertSeparatorNodes config admin maliciousAction (fromJust (toPubKeyHash config.owner)) node.key insertedNodes

nodeInsertionTest :: NodeTestConfig -> LaunchpadConfig -> Run ()
nodeInsertionTest NodeTestConfig {..} config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let (sortedKey1, sortedKey2, sortedKey3) = sortedKeys (userWallet1, userWallet2, userWallet3) userChoice maliciousAction
      node = case firstNodeChoice of
        LastNode -> Node (Just sortedKey1) Nothing config.defaultStartTime 100
        MiddleNode -> Node (Just sortedKey1) (Just sortedKey3) config.defaultStartTime 100
        SingleHeadNode -> Node Nothing Nothing 0 0
        HeadNode -> Node Nothing (Just sortedKey3) 0 0
      newNode t = Node (Just sortedKey2) node.next t 200
  createNodeRefScript config admin
  createFirstProjectTokensHolder
    config
    admin
    0
    ( case maliciousAction of
        NoTokensHolderRef -> SkipToken
        _ -> AddToken
    )
  waitUntil config.defaultStartTime
  waitNSlots 1 -- so the lower approximation is right as well
  createNode config admin node AddToken
  when (timing == Just AfterContributionPhase) $ do
    waitUntil config.endTime
    waitNSlots 1 -- so the lower approximation is right as well
  insertNode config admin maliciousAction node.key newNode tier

nodeIndexTest :: NodeTestConfig -> Integer -> LaunchpadConfig -> Run ()
nodeIndexTest NodeTestConfig {..} index config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser

  let (sortedPkh1, sortedPkh2) = case sort [(userWallet1), (userWallet2)] of
        [a, b] -> (a, b)
        _ -> error "sort failed"

  createNodeRefScript config admin
  createNode config admin (Node (Just (unwrapPubKeyHash sortedPkh1, 0)) Nothing 0 0) AddToken
  createFirstProjectTokensHolder config admin 2 AddToken

  waitUntil config.defaultStartTime
  waitNSlots 2

  let newNode t = Node (Just (unwrapPubKeyHash sortedPkh2, index)) Nothing t 20000
  insertNode config admin None (Just (unwrapPubKeyHash sortedPkh1, 0)) newNode tier

nodeIndexSeparatorTest :: Integer -> LaunchpadConfig -> Run ()
nodeIndexSeparatorTest index config = do
  _ <- setupWallets config
  admin <- getMainUser

  let nodeKey i = ("a", i)
      headNode = Node Nothing Nothing 0 0
      separatorNode i t = Node (Just (nodeKey i)) headNode.next t 0

  createFirstProjectTokensHolder config admin 0 AddToken
  createNodeRefScript config admin

  createNode config admin headNode AddToken
  insertSeparatorNodes config admin None (fromJust (toPubKeyHash config.owner)) headNode.key [separatorNode index]
