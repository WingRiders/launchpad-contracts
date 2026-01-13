module Integration.Tests where

import Integration.Launchpad.CommitFoldTest (commitFoldTests)
import Integration.Launchpad.FailProofTest (failProofTests)
import Integration.Launchpad.LaunchpadTest (launchpadTests)
import Integration.Launchpad.NodeTest (nodeTests)
import Integration.Launchpad.PoolProofTest (poolProofTests)
import Integration.Launchpad.ProjectTokensHolderFinalTest (projectTokensHolderFinalTests)
import Integration.Launchpad.RewardsFoldTest (rewardsFoldTests)
import Integration.Launchpad.RewardsHolderTest (rewardsHolderTests)
import Test.Tasty

integrationTestsLaunchpad :: TestTree
integrationTestsLaunchpad =
  testGroup
    "Integration tests - Launchpad"
    [ launchpadTests
    , failProofTests
    , projectTokensHolderFinalTests
    , rewardsHolderTests
    , poolProofTests
    , commitFoldTests
    , rewardsFoldTests
    , nodeTests
    ]
