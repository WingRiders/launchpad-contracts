module Integration.Launchpad.FailProofTest where

import Integration.Launchpad.CommitFold (createCommitFold, createCommitFoldRefScript)
import Integration.Launchpad.FailProof
import Integration.Launchpad.Node hiding (None)
import Integration.Launchpad.ProjectTokensHolderFirst
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util
import Launchpad.Types
import Plutus.Model
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import Test.Tasty (
  TestTree,
  testGroup,
 )

failProofTests :: TestTree
failProofTests =
  testGroup
    "FailProof"
    [ testGroup "Create FailProof" createTests
    ]

createTests :: [TestTree]
createTests =
  [ good defaultLaunchpadConfig "FailProof can be created when the min commitment is not reached" (fail_launchpad None)
  , bad defaultLaunchpadConfig {projectMinCommitment = 0} "FailProof can not be created when the min commitment is reached" (fail_launchpad None)
  , bad defaultLaunchpadConfig "FailProof can not be created when CommitFold token is not burned" (fail_launchpad NoBurningFold)
  , bad defaultLaunchpadConfig "FailProof can not be created when Holder token is not burned" (fail_launchpad NoBurningHolder)
  , bad defaultLaunchpadConfig "FailProof can not be created when Node token is not burned" (fail_launchpad NoBurningNode)
  , bad defaultLaunchpadConfig "FailProof can not be created when more tokens present" (fail_launchpad MultipleTokens)
  ]

fail_launchpad :: MaliciousFailProofAction -> LaunchpadConfig -> Run ()
fail_launchpad action config = do
  Wallets {..} <- setupWallets config
  admin <- getMainUser
  createNodeRefScript config admin
  createFirstProjectTokensHolderRefScript config admin
  createCommitFoldRefScript config admin
  createFirstProjectTokensHolder config admin 0 AddToken
  let nodeNext = (unwrapPubKeyHash userWallet1, 0)
  createNode config admin (Node Nothing (Just nodeNext) 0 0) AddToken
  createCommitFold
    config
    admin
    ( CommitFoldDatum
        { nodeScriptHash = toValidatorHash (nodeValidator config)
        , next = Nothing
        , committed = 1
        , cutoffKey = Nothing
        , cutoffTime = Nothing
        , overcommitted = 0
        , nodeCount = 5
        , owner = pubKeyHashAddress admin
        }
    )
    AddToken
  initFailProof action config launchpadOwner
