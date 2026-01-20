{-# LANGUAGE BlockArguments #-}

module Integration.Launchpad.CommitFoldTest (commitFoldTests) where

import Control.Monad (forM_, when)
import Data.List (sort)
import Integration.Launchpad.CommitFold
import Integration.Launchpad.Node hiding (None)
import Integration.Launchpad.Options (RunLimitTests (..), ThoroughTests (..))
import Integration.Mock
import Integration.Util
import Launchpad.Node qualified as N
import Launchpad.Types
import Plutus.Model
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V2 (POSIXTime, PubKeyHash)
import Test.Tasty

shortMockedKey :: PubKeyHash
shortMockedKey = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

regularMockedKey :: PubKeyHash
regularMockedKey = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

commitFoldTests :: TestTree
commitFoldTests = askOption \(ThoroughTests thorough) ->
  testGroup "CommitFold" $
    [ good defaultLaunchpadConfig "The commit fold can be created" createCommitFoldTest
    , bad defaultLaunchpadConfig "Fails to create the commit fold when the launchpad hasn't ended yet" createCommitFoldLaunchpadNotEnded
    , bad defaultLaunchpadConfig "Fails to create the commit fold when the next field is invalid" createCommitFoldInvalidNext
    , bad defaultLaunchpadConfig "Fails to create the commit fold when the commit fold datum is initialized with non-zero committed amount" createCommitFoldInvalidCommitted
    , good defaultLaunchpadConfig "Can fold over an eligible node, a cutoff node and an ineligible node" (foldOverNodes' [ThreePresale, FourDefault, LastDefault] (Just FourDefault))
    , bad defaultLaunchpadConfig "Fails when the provided node is not the next expected one" foldOverWrongNode
    , commitFoldLimitTests
    ]
      <> case thorough of
        False -> []
        True ->
          [ good defaultLaunchpadConfig "The commit fold can be created if cutoff key is 28B" (createCommitFoldWithCutoff (Just (unwrapPubKeyHash regularMockedKey, 0)) (Just 10))
          , bad defaultLaunchpadConfig "The commit fold can not be created if cutoff key is 27B" (createCommitFoldWithCutoff (Just (unwrapPubKeyHash shortMockedKey, 0)) (Just 10))
          , bad defaultLaunchpadConfig "The commit fold can not be created if cutoff key is Nothing and cutoff time Just" (createCommitFoldWithCutoff Nothing (Just 10))
          , bad defaultLaunchpadConfig "The commit fold can not be created if cutoff key is Just and cutoff time Nothing" (createCommitFoldWithCutoff (Just (unwrapPubKeyHash regularMockedKey, 0)) Nothing)
          , bad defaultLaunchpadConfig "Fails to create the commit fold when the node has no token" createCommitFoldWithoutToken
          , bad defaultLaunchpadConfig "Fails to create the commit fold when the node is not the head node" createCommitFoldNotHeadNode
          , bad defaultLaunchpadConfig "Fails to create the commit fold with a negative overcommitted amount" createCommitFoldNegativeOvercommitted
          , good defaultLaunchpadConfig "Can fold over a presale ineligible node" (foldOverNodes' [TwoPresale] (Just OnePresale))
          , good defaultLaunchpadConfig "Can fold over a presale eligible node" (foldOverNodes' [TwoPresale] Nothing)
          , good defaultLaunchpadConfig "Can fold over a presale cutoff node" (foldOverNodes' [TwoPresale] (Just TwoPresale))
          , good defaultLaunchpadConfig "Can fold over a default eligible node" (foldOverNodes' [FourDefault] (Just LastDefault))
          , good defaultLaunchpadConfig "Can fold over a default cutoff node" (foldOverNodes' [FourDefault] (Just FourDefault))
          , good defaultLaunchpadConfig "Can fold over a cutoff node and an ineligible node" (foldOverNodes' [FourDefault, LastDefault] (Just FourDefault))
          , good defaultLaunchpadConfig "Can fold over a default ineligible node" (foldOverNodes' [FourDefault] (Just OnePresale))
          , good defaultLaunchpadConfig "Can fold over a default last ineligible node" (foldOverNodes' [LastDefault] (Just OnePresale))
          , good defaultLaunchpadConfig "Can fold over a default last eligible node" (foldOverNodes' [LastDefault] Nothing)
          , good defaultLaunchpadConfig "Can fold over a default last cutoff node" (foldOverNodes' [LastDefault] (Just LastDefault))
          , good defaultLaunchpadConfig "Can fold over a presale last ineligible node" (foldOverNodes' [LastPresale] (Just OnePresale))
          , good defaultLaunchpadConfig "Can fold over a presale last eligible node" (foldOverNodes' [LastPresale] Nothing)
          , good defaultLaunchpadConfig "Can fold over a presale last cutoff node" (foldOverNodes' [LastPresale] (Just LastPresale))
          , good defaultLaunchpadConfig "Can fold over two eligible middle nodes" (foldOverNodes' [OnePresale, TwoPresale] Nothing)
          , good defaultLaunchpadConfig "Can fold over two eligible nodes including the last one" (foldOverNodes' [FourDefault, LastDefault] Nothing)
          , good defaultLaunchpadConfig "Can fold over an eligible node and a cutoff node" (foldOverNodes' [FourDefault, LastDefault] (Just LastDefault))
          , bad defaultLaunchpadConfig "Fails when the provided node is the head node and the fold is finished" foldOverHeadNode
          , bad defaultLaunchpadConfig "Fails when the node folded over doesn't have the node token" foldOverNodeWithoutToken
          , bad defaultLaunchpadConfig "Fails when the new datum has invalid next field" foldGetInvalidNext
          , bad defaultLaunchpadConfig "Fails when the new datum has changed the owner" foldGetInvalidOwner
          , bad defaultLaunchpadConfig "Fails when the new datum has changed the cutoff key" foldGetInvalidCutoffKey
          , bad defaultLaunchpadConfig "Fails when the new datum has changed the cutoff time" foldGetInvalidCutoffTime
          , bad defaultLaunchpadConfig "Fails when the new datum has changed the overcommitted amount" foldGetInvalidOvercommitted
          , bad defaultLaunchpadConfig "Fails when the new datum has changed the node script hash" foldGetInvalidNodeScriptHash
          , bad defaultLaunchpadConfig "Fails when the new datum has an incorrect committed value" foldGetInvalidCommitted
          , bad defaultLaunchpadConfig "Fails when the commit fold doesn't have the token" foldWithoutToken
          , bad defaultLaunchpadConfig "Fails when double satisfaction is tried" doubleSatisfyCommitFoldSpendingTest
          ]

commitFoldLimitTests :: TestTree
commitFoldLimitTests = askOption $ \case
  RunLimitTests True ->
    testGroup
      "Commit fold limits"
      [ good defaultLaunchpadConfig "Can fold over 61 default nodes" (foldOverNodes 0 61)
      , good defaultLaunchpadConfig "Can fold over 21 presale nodes and 40 default nodes" (foldOverNodes 21 40)
      , good defaultLaunchpadConfig "Can fold over 61 presale nodes" (foldOverNodes 61 0)
      , bad defaultLaunchpadConfig "Fails to fold over 62 default nodes" (foldOverNodes 0 62)
      , bad defaultLaunchpadConfig "Fails to fold over 22 presale nodes and 40 default nodes" (foldOverNodes 22 40)
      , bad defaultLaunchpadConfig "Fails to fold over 62 presale nodes" (foldOverNodes 62 0)
      ]
  RunLimitTests False -> testGroup "Commit fold limits" []

foldOverNodes :: Int -> Int -> LaunchpadConfig -> Run ()
foldOverNodes tieredNodesCount defaultNodesCount config = do
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
      commitFold =
        CommitFoldDatum
          { nodeScriptHash = N.nodeScriptValidatorHash (nodeConfig config)
          , next = Just (head sortedKeys)
          , committed = 0
          , cutoffKey = Just (head sortedKeys)
          , cutoffTime = Just 1
          , overcommitted = 1_000
          , nodeCount = 1
          , owner = pubKeyHashAddress admin
          }

  createNodeRefScript config admin
  forM_ tieredNodes createPresaleNode
  forM_ defaultNodes createDefaultNode
  createCommitFold config admin commitFold AddToken
  commitFoldOver config admin Nothing (Just <$> sortedKeys) None

foldWithoutToken :: LaunchpadConfig -> Run ()
foldWithoutToken config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
  createNode config admin (Node (Just nodeKey) Nothing 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just nodeKey)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    SkipToken
  commitFoldOver
    config
    admin
    Nothing
    [Just nodeKey]
    None

foldOverNodeWithoutToken :: LaunchpadConfig -> Run ()
foldOverNodeWithoutToken config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) SkipToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just sortedKey2)
            100
            Nothing
            Nothing
            0
            1
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidCommitted :: LaunchpadConfig -> Run ()
foldGetInvalidCommitted config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just sortedKey2)
            101
            Nothing
            Nothing
            0
            2
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidNodeScriptHash :: LaunchpadConfig -> Run ()
foldGetInvalidNodeScriptHash config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig (config {startTime = config.startTime + 1})))
            (Just sortedKey2)
            100
            Nothing
            Nothing
            0
            2
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidOvercommitted :: LaunchpadConfig -> Run ()
foldGetInvalidOvercommitted config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just sortedKey2)
            100
            Nothing
            Nothing
            1
            2
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidCutoffTime :: LaunchpadConfig -> Run ()
foldGetInvalidCutoffTime config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just sortedKey2)
            100
            Nothing
            (Just 1)
            0
            2
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidCutoffKey :: LaunchpadConfig -> Run ()
foldGetInvalidCutoffKey config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just sortedKey2)
            100
            (Just sortedKey2)
            Nothing
            0
            2
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidOwner :: LaunchpadConfig -> Run ()
foldGetInvalidOwner config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just sortedKey2)
            100
            Nothing
            Nothing
            0
            2
            (pubKeyHashAddress userWallet1)
        )
    )
    [Just sortedKey1]
    None

foldGetInvalidNext :: LaunchpadConfig -> Run ()
foldGetInvalidNext config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
      (sortedKey1, sortedKey2) = if nodeKey1 < nodeKey2 then (nodeKey1, nodeKey2) else (nodeKey2, nodeKey1)
  createNode config admin (Node (Just sortedKey1) (Just sortedKey2) 2 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just sortedKey1)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver
    config
    admin
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            Nothing
            100
            Nothing
            Nothing
            0
            2
            (pubKeyHashAddress admin)
        )
    )
    [Just sortedKey1]
    None

foldOverHeadNode :: LaunchpadConfig -> Run ()
foldOverHeadNode config = do
  admin <- getMainUser
  createHeadNode config admin AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        Nothing
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver config admin Nothing [Nothing] None

data NodeNumber
  = OnePresale
  | TwoPresale
  | ThreePresale
  | FourDefault
  | LastDefault
  | LastPresale
  deriving (Eq)
type CutoffNode = Maybe NodeNumber

foldOverNodes' :: [NodeNumber] -> CutoffNode -> LaunchpadConfig -> Run ()
foldOverNodes' foldedNodes cutoffNode config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser

  let (sortedPkh1, sortedPkh2, sortedPkh3) = case sort [(userWallet1), (userWallet2), (userWallet3)] of
        [a, b, c] -> (a, b, c)
        _ -> error "sort failed"

      node1 = Node (Just (unwrapPubKeyHash sortedPkh1, 0)) node2.key 0 20000
      node2 = Node (Just (unwrapPubKeyHash sortedPkh2, 0)) node3.key 1 20000
      node3 = Node (Just (unwrapPubKeyHash sortedPkh3, 0)) node4.key 2 20000
      node4 = Node (Just (unwrapPubKeyHash sortedPkh3, 1)) node5.key 3 20000
      node5 = Node (Just (unwrapPubKeyHash sortedPkh3, 2)) Nothing 4 20000

      nodeOf OnePresale = node1
      nodeOf TwoPresale = node2
      nodeOf ThreePresale = node3
      nodeOf FourDefault = node4
      nodeOf LastDefault = node5
      nodeOf LastPresale = node5

      foldedKeys = map (key . nodeOf) foldedNodes
      cutoffKey = key . nodeOf =<< cutoffNode
      cutoffTime = createdTime . nodeOf <$> cutoffNode

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

  createCommitFold
    config
    admin
    ( CommitFoldDatum
        { nodeScriptHash = N.nodeScriptValidatorHash (nodeConfig config)
        , next = head foldedKeys
        , committed = 0
        , cutoffKey
        , cutoffTime
        , overcommitted = 0
        , nodeCount = 1
        , owner = pubKeyHashAddress admin
        }
    )
    AddToken
  commitFoldOver config admin Nothing foldedKeys None

foldOverWrongNode :: LaunchpadConfig -> Run ()
foldOverWrongNode config = do
  Wallets {..} <- setupWallets config

  admin <- getMainUser
  let nodeKey1 = (unwrapPubKeyHash userWallet1, 0)
      nodeKey2 = (unwrapPubKeyHash userWallet2, 0)
  createNode config admin (Node (Just nodeKey2) Nothing 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just nodeKey1)
        0
        Nothing
        Nothing
        0
        2
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver config admin Nothing [Just nodeKey2] None

createCommitFoldInvalidCommitted :: LaunchpadConfig -> Run ()
createCommitFoldInvalidCommitted config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin AddToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold
    config
    (pubKeyHashAddress launchpadOwner)
    userWallet2
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            Nothing
            1
            Nothing
            Nothing
            0
            1
            (pubKeyHashAddress launchpadOwner)
        )
    )
    Nothing

createCommitFoldLaunchpadNotEnded :: LaunchpadConfig -> Run ()
createCommitFoldLaunchpadNotEnded config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin AddToken
  initCommitFold config (pubKeyHashAddress launchpadOwner) userWallet2 Nothing Nothing

createCommitFoldInvalidNext :: LaunchpadConfig -> Run ()
createCommitFoldInvalidNext config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin AddToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold
    config
    (pubKeyHashAddress launchpadOwner)
    userWallet2
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            (Just (unwrapPubKeyHash userWallet1, 0))
            0
            Nothing
            Nothing
            0
            1
            (pubKeyHashAddress launchpadOwner)
        )
    )
    Nothing

createCommitFoldNotHeadNode :: LaunchpadConfig -> Run ()
createCommitFoldNotHeadNode config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
  createNode config admin (Node (Just nodeKey) Nothing 0 100) AddToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold config (pubKeyHashAddress launchpadOwner) userWallet2 Nothing (Just nodeKey)

createCommitFoldWithoutToken :: LaunchpadConfig -> Run ()
createCommitFoldWithoutToken config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin SkipToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold config (pubKeyHashAddress launchpadOwner) userWallet2 Nothing Nothing

createCommitFoldNegativeOvercommitted :: LaunchpadConfig -> Run ()
createCommitFoldNegativeOvercommitted config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin AddToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold
    config
    (pubKeyHashAddress launchpadOwner)
    userWallet2
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            Nothing
            0
            (Just (unwrapPubKeyHash userWallet1, 0))
            (Just 10)
            (-1)
            1
            (pubKeyHashAddress launchpadOwner)
        )
    )
    Nothing

createCommitFoldTest :: LaunchpadConfig -> Run ()
createCommitFoldTest config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin AddToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold config (pubKeyHashAddress launchpadOwner) userWallet2 Nothing Nothing

doubleSatisfyCommitFoldSpendingTest :: LaunchpadConfig -> Run ()
doubleSatisfyCommitFoldSpendingTest config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  let nodeKey = (unwrapPubKeyHash userWallet1, 0)
  createNode config admin (Node (Just nodeKey) Nothing 0 100) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just nodeKey)
        0
        Nothing
        Nothing
        0
        1
        (pubKeyHashAddress admin)
    )
    AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        (N.nodeScriptValidatorHash (nodeConfig config))
        (Just nodeKey)
        0
        Nothing
        Nothing
        0
        2
        (pubKeyHashAddress admin)
    )
    AddToken
  commitFoldOver config admin Nothing [Just nodeKey] DoubleSatisfy

createCommitFoldWithCutoff :: Maybe NodeKey -> Maybe POSIXTime -> LaunchpadConfig -> Run ()
createCommitFoldWithCutoff key time config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createHeadNode config admin AddToken
  waitUntil config.endTime
  waitNSlots 10 -- so the lower time approximation is after the end time as well
  initCommitFold
    config
    (pubKeyHashAddress launchpadOwner)
    userWallet2
    ( Just
        ( CommitFoldDatum
            (N.nodeScriptValidatorHash (nodeConfig config))
            Nothing
            0
            key
            time
            1
            1
            (pubKeyHashAddress launchpadOwner)
        )
    )
    Nothing
