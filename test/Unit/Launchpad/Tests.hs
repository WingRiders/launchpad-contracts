module Unit.Launchpad.Tests where

import Test.Tasty
import Unit.Launchpad.CommitFold
import Unit.Launchpad.Node
import Unit.Launchpad.PoolProof
import Unit.Launchpad.RewardsFold
import Unit.Launchpad.UtilFunctions

launchpadUnitTests :: TestTree
launchpadUnitTests =
  testGroup
    "Launchpad"
    [ utilityFunctionsTests
    , nodeMintingPolicyTests
    , nodeUtilsTests
    , poolProofMintingPolicyTests
    , commitFoldTests
    , rewardsFoldTests
    ]
