{-# LANGUAGE BlockArguments #-}

module Integration.Launchpad.LaunchpadTest where

import Control.Monad (forM_, when)
import Data.Default (Default (..))
import Data.List (sort)
import Data.Maybe (fromJust)
import Integration.Launchpad.CommitFold (commitFoldOver, createCommitFoldRefScript, initCommitFold)
import Integration.Launchpad.CommitFold qualified as CF
import Integration.Launchpad.FailProof (initFailProof)
import Integration.Launchpad.FailProof qualified as FP
import Integration.Launchpad.Launchpad
import Integration.Launchpad.Node hiding (None)
import Integration.Launchpad.Node qualified as Node
import Integration.Launchpad.Options (ThoroughTests (..))
import Integration.Launchpad.PoolProof (createPoolProof, createPoolUtxo)
import Integration.Launchpad.PoolProof qualified as PP
import Integration.Launchpad.ProjectTokensHolderFinal (spendHolderCreatePool, spendHolderFundsToDao)
import Integration.Launchpad.ProjectTokensHolderFinal qualified as PTHF
import Integration.Launchpad.ProjectTokensHolderFirst
import Integration.Launchpad.RewardsFold (RewardsFoldApplicationConfig, RewardsFoldIteration (..), createRewardsFoldRefScript, initRewardsFold, iteration, nodeKeys, rewardsFoldOver)
import Integration.Launchpad.RewardsHolder (spendRewardsHolder)
import Integration.Launchpad.RewardsHolder qualified as RH
import Integration.Mock
import Integration.Util
import Launchpad.Types
import Plutus.Model
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, toPubKeyHash)
import PlutusLedgerApi.V2 (PubKeyHash)
import Test.Tasty (
  TestTree,
  askOption,
  testGroup,
 )

launchpadTests :: TestTree
launchpadTests = testGroup "Launchpad" [completeTests, createTests, cancelTests]

createTests :: TestTree
createTests = askOption \(ThoroughTests thorough) ->
  testGroup "Create Launchpad" $
    [good defaultLaunchpadConfig "Launchpad owner can create ProjectTokens holder with head Node" (create_launchpad None)]
      <> case thorough of
        False -> []
        True ->
          [ bad defaultLaunchpadConfig "Not current time in Node Datum results in error" (create_launchpad CreateTimeNotCurrent)
          , bad defaultLaunchpadConfig "Key being Just in Node Datum results in error" (create_launchpad KeyJust)
          , bad defaultLaunchpadConfig "Next being Just in Node Datum results in error" (create_launchpad NextJust)
          , bad defaultLaunchpadConfig "Non-zero commitment in Node Datum results in error" (create_launchpad NonZeroCommitted)
          , bad defaultLaunchpadConfig "Missing owner signature results in error" (create_launchpad NoOwnerSignature)
          , bad defaultLaunchpadConfig "Wrong Node ScriptHash TokenName results in error" (create_launchpad WrongNodeTokenName)
          , bad defaultLaunchpadConfig "Wrong Holder ScriptHash TokenName results in error" (create_launchpad WrongHolderTokenName)
          , bad defaultLaunchpadConfig "Additional Node token given to user results in error" (create_launchpad AdditionalNodeTokenToUser)
          , bad defaultLaunchpadConfig "Additional Holder token given to user results in error" (create_launchpad AdditionalHolderTokenToUser)
          , bad defaultLaunchpadConfig "Locking less project tokens results in error" (create_launchpad LessProjectTokensLocked)
          , bad defaultLaunchpadConfig {startTime = 1} "Setting start time in the past results in error" (create_launchpad StartTimeInPast)
          ]

cancelTests :: TestTree
cancelTests = askOption \(ThoroughTests thorough) ->
  testGroup "Cancel Launchpad" $
    [ good defaultLaunchpadConfig "Launchpad owner can cancel the launchpad before start time" (cancel_launchpad (None, None))
    , bad defaultLaunchpadConfig "Launchpad owner cannot cancel the launchpad after start time" (cancel_launchpad (None, CancelTimeAfterStart))
    ]
      <> case thorough of
        False -> []
        True ->
          [ bad defaultLaunchpadConfig "Launchpad owner cannot cancel the launchpad without the signature" (cancel_launchpad (None, NoOwnerSignature))
          , bad defaultLaunchpadConfig "Fails when not burning token on cancellation" (cancel_launchpad (None, TokenNotBurned))
          ]

completeTests :: TestTree
completeTests = askOption \(ThoroughTests thourough) ->
  testGroup "Whole Launchpad" $
    [ good defaultLaunchpadConfig "Whole launchpad flow - Wr - Success" (run_launchpad NoPool)
    , good defaultLaunchpadConfig {splitBps = 0} "Whole launchpad flow - Sundae - Success" (run_launchpad NoPool)
    , good defaultLaunchpadConfig {splitBps = 5_000} "Whole launchpad flow - Wr and Sundae - Success" (run_launchpad NoPool)
    ]
      <> case thourough of
        False -> []
        True ->
          [ good defaultLaunchpadConfig "Whole launchpad flow - Wr - Pool exists" (run_launchpad PoolExists)
          , good
              defaultLaunchpadConfig {contributionEndTime = defaultLaunchpadConfig.withdrawalEndTime}
              "Whole launchpad flow - Wr - No Pool - contribution end time = withdrawal end time"
              (run_launchpad NoPool)
          , good defaultLaunchpadConfig {projectMinCommitment = 80001} "Whole launchpad flow - Wr - Fail Launchpad" (run_launchpad Fails)
          , -- NOTE: the min supported collateral is 4 ada
            good defaultLaunchpadConfig {collateral = 4_000_000} "Whole launchpad flow - Wr - No Pool - 4 ADA collateral" (run_launchpad NoPool)
          , good defaultLaunchpadConfig {collateral = 4_000_000} "Whole launchpad flow - Wr - Pool Exists - 4 ADA collateral" (run_launchpad PoolExists)
          ]

cancel_launchpad :: (MaliciousLaunchpadAction, MaliciousLaunchpadAction) -> LaunchpadConfig -> Run ()
cancel_launchpad (createAction, cancelAction) config = do
  Wallets {..} <- setupWallets config
  utxos <- utxoAt launchpadOwner
  let (outRef, _) = head utxos

  createLaunchpad createAction config {starter = outRef} launchpadOwner
  case cancelAction of
    CancelTimeAfterStart -> waitUntil config.startTime
    _ -> pure ()
  cancelLaunchpad cancelAction config {starter = outRef} False launchpadOwner

create_launchpad :: MaliciousLaunchpadAction -> LaunchpadConfig -> Run ()
create_launchpad action config = do
  Wallets {..} <- setupWallets config
  utxos <- utxoAt launchpadOwner
  let (outRef, _) = head utxos

  createLaunchpad action config {starter = outRef} launchpadOwner

run_launchpad :: LaunchpadAction -> LaunchpadConfig -> Run ()
run_launchpad action initConfig = do
  wallets@Wallets {..} <- setupWallets initConfig
  admin <- getMainUser
  utxos <- utxoAt launchpadOwner
  let (outRef, _) = head utxos
  let config = initConfig {starter = outRef}

      (sortedPkh1, sortedPkh2, sortedPkh3) = case sort [(userWallet1), (userWallet2), (userWallet3)] of
        [a, b, c] -> (a, b, c)
        _ -> error "sort failed"

  let separatorNodeKey i = ("0", i)
      separatorNode t = Node (Just (separatorNodeKey 0)) Nothing t 0

      node1 t = Node (Just (unwrapPubKeyHash sortedPkh1, 0)) Nothing t 20000
      node2 t = Node (Just (unwrapPubKeyHash sortedPkh2, 0)) Nothing t 20000
      node3 t = Node (Just (unwrapPubKeyHash sortedPkh3, 0)) Nothing t 20000
      node4 t = Node (Just (unwrapPubKeyHash sortedPkh3, 1)) Nothing t 20000

  createLaunchpad None config launchpadOwner
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createCommitFoldRefScript config admin

  insertSeparatorNodes config admin Node.None (fromJust (toPubKeyHash config.owner)) Nothing [separatorNode]

  waitUntil config.defaultStartTime
  waitNSlots 2

  insertNode config admin Node.None (Just (separatorNodeKey 0)) node1 Presale
  insertNode config admin Node.None (Just (unwrapPubKeyHash sortedPkh1, 0)) node2 Presale
  insertNode config admin Node.None (Just (unwrapPubKeyHash sortedPkh2, 0)) node3 Presale
  insertNode config admin Node.None (Just (unwrapPubKeyHash sortedPkh3, 0)) node4 Default

  waitUntil config.withdrawalEndTime
  waitNSlots 2

  initCommitFold config (pubKeyHashAddress admin) userWallet2 Nothing Nothing
  commitFoldOver config admin Nothing [Just (separatorNodeKey 0), Just (unwrapPubKeyHash sortedPkh1, 0), Just (unwrapPubKeyHash sortedPkh2, 0), Just (unwrapPubKeyHash sortedPkh3, 0), Just (unwrapPubKeyHash sortedPkh3, 1)] CF.None

  let foldAppConfig =
        def
          { nodeKeys =
              [ Just (separatorNodeKey 0)
              , Just (unwrapPubKeyHash sortedPkh1, 0)
              , Just (unwrapPubKeyHash sortedPkh2, 0)
              , Just (unwrapPubKeyHash sortedPkh3, 0)
              , Just (unwrapPubKeyHash sortedPkh3, 1)
              ]
          , iteration = LastNode
          }

  case action of
    PoolExists -> run_launchpad_pool_exists config wallets [sortedPkh1, sortedPkh2, sortedPkh3] admin foldAppConfig
    NoPool -> run_launchpad_no_pool config [sortedPkh1, sortedPkh2, sortedPkh3] admin launchpadOwner foldAppConfig
    Fails -> run_launchpad_fail config [(sortedPkh1, 0), (sortedPkh2, 0), (sortedPkh3, 0), (sortedPkh3, 1)] launchpadOwner

run_launchpad_rewards_fold_phase :: LaunchpadConfig -> RewardsFoldApplicationConfig -> PubKeyHash -> Run ()
run_launchpad_rewards_fold_phase config foldAppConfig admin = do
  initRewardsFold config admin Nothing Nothing AddToken
  createRewardsFoldRefScript config admin
  rewardsFoldOver config admin foldAppConfig

run_launchpad_fail :: LaunchpadConfig -> [(PubKeyHash, Integer)] -> PubKeyHash -> Run ()
run_launchpad_fail config nodes launchpadOwner = do
  initFailProof FP.None config launchpadOwner
  forM_ nodes $ \(pkh, index) -> do
    reclaimNode config UserReclaims pkh (unwrapPubKeyHash pkh, index)

run_launchpad_pool_exists :: LaunchpadConfig -> Wallets -> [PubKeyHash] -> PubKeyHash -> RewardsFoldApplicationConfig -> Run ()
run_launchpad_pool_exists config Wallets {..} sortedPkhs admin foldAppConfig = do
  run_launchpad_rewards_fold_phase config foldAppConfig admin

  when (config.splitBps < 10_000) do
    fail "With Sundae it's always possible to create a pool"

  when (config.splitBps > 0) do
    createPoolUtxo PP.None Wr config poolInitWallet
    createPoolProof PP.None Wr config userWallet1

  spendHolderFundsToDao PTHF.None config Wr admin launchpadOwner
  forM_ sortedPkhs $ \pkh -> do
    spendRewardsHolder RH.None config pkh

run_launchpad_no_pool :: LaunchpadConfig -> [PubKeyHash] -> PubKeyHash -> PubKeyHash -> RewardsFoldApplicationConfig -> Run ()
run_launchpad_no_pool config sortedPkhs admin launchpadOwner foldAppConfig = do
  run_launchpad_rewards_fold_phase config foldAppConfig admin

  when (config.splitBps > 0) do
    spendHolderCreatePool PTHF.None Wr config admin launchpadOwner
    createPoolProof PP.None Wr config admin

  when (config.splitBps < 10_000) do
    spendHolderCreatePool PTHF.None Sundae config admin launchpadOwner
    createPoolProof PP.None Sundae config admin

  forM_ sortedPkhs $ \pkh -> do
    spendRewardsHolder RH.None config pkh
