{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
  This executable should exports all contracts
-}
module Export where

import Control.Exception
import Data.Aeson (ToJSON)
import ExportUtils
import GHC.Generics (Generic)
import Launchpad.CommitFold (commitFoldScript)
import Launchpad.Constants qualified as C
import Launchpad.FailProof (failProofScript)
import Launchpad.Mint.CommitFold (commitFoldPolicyScript)
import Launchpad.Mint.FailProof (failProofPolicyScript)
import Launchpad.Mint.Node (nodePolicyScript)
import Launchpad.Mint.PoolProof (poolProofPolicyScript)
import Launchpad.Mint.ProjectTokensHolder (projectTokensHolderPolicyScript)
import Launchpad.Mint.RewardsFold (rewardsFoldPolicyScript)
import Launchpad.Node (nodeScript)
import Launchpad.PoolProof (poolProofScript)
import Launchpad.ProjectTokensHolderFinal qualified as F
import Launchpad.ProjectTokensHolderFirst qualified as First
import Launchpad.RewardsFold (rewardsFoldScript)
import Launchpad.RewardsHolder (rewardsHolderScript)
import Other.FixedSupplyPolicy (fixedSupplyPolicyUnapplied)
import Other.Mint.AlwaysFails (alwaysFailsMintingPolicy)
import Other.Mint.FreeNft (freeNftPolicyScript)
import Other.RefScriptCarrier (refScriptCarrierScript)
import Other.Vesting (vestingScriptValidator)
import System.Directory

data ExportInfo = ExportInfo
  { validators :: [ValidatorMeasurement]
  , policies :: [PolicyMeasurement]
  , launchpadConstants :: LaunchpadConstants
  }
  deriving (Show, Generic, ToJSON)

data LaunchpadConstants = LaunchpadConstants
  { minNodeIndex :: Integer
  , maxNodeIndex :: Integer
  , separatorNodeKeyLength :: Integer
  , nodesInactivityPeriod :: Integer
  , emergencyWithdrawalPeriod :: Integer
  }
  deriving (Show, Generic, ToJSON)

outDir :: String
outDir = "artifacts/"

main :: IO ()
main = do
  catch (removeDirectoryRecursive outDir) (\(_ :: IOException) -> pure ())
  createDirectoryIfMissing True outDir

  commitFoldMeasurement <-
    exportAllValidators
      [(commitFoldScript, outDir, "commit-fold", V2)]

  nodeMeasurement <-
    exportAllValidators
      [(nodeScript, outDir, "node", V2)]

  poolProofMeasurement <-
    exportAllValidators
      [(poolProofScript, outDir, "pool-proof", V2)]

  projectTokensHolderFinalMeasurement <-
    exportAllValidators
      [(F.projectTokensHolderScript, outDir, "project-tokens-holder-final", V2)]

  projectTokensHolderFirstMeasurement <-
    exportAllValidators
      [(First.projectTokensHolderScript, outDir, "project-tokens-holder-first", V2)]

  rewardsFoldMeasurement <-
    exportAllValidators
      [(rewardsFoldScript, outDir, "rewards-fold", V2)]

  rewardsHolderMeasurement <-
    exportAllValidators
      [(rewardsHolderScript, outDir, "rewards-holder", V2)]

  failProofMeasurement <-
    exportAllValidators
      [(failProofScript, outDir, "fail-proof", V2)]

  commitFoldTokenMeasurement <-
    exportAllPolicies
      [(commitFoldPolicyScript, outDir, "commit-fold-policy", V2)]

  nodeTokenMeasurement <-
    exportAllPolicies
      [(nodePolicyScript, outDir, "node-policy", V2)]

  poolProofTokenMeasurement <-
    exportAllPolicies
      [(poolProofPolicyScript, outDir, "pool-proof-policy", V2)]

  projectTokensHolderTokenMeasurement <-
    exportAllPolicies
      [(projectTokensHolderPolicyScript, outDir, "project-tokens-holder-policy", V2)]

  rewardsFoldTokenMeasurement <-
    exportAllPolicies
      [(rewardsFoldPolicyScript, outDir, "rewards-fold-policy", V2)]

  failProofTokenMeasurement <-
    exportAllPolicies
      [(failProofPolicyScript, outDir, "fail-proof-policy", V2)]

  alwaysFailsMeasurement <- exportAllPolicies [(alwaysFailsMintingPolicy, outDir, "always-fails-token", V2)]

  refScriptCarrierMeasurement <-
    exportAllValidators
      [(refScriptCarrierScript, outDir, "ref-script-carrier", V2)]

  vestingMeasurement <-
    exportAllValidators
      [(vestingScriptValidator, outDir, "vesting", V2)]

  freeNftMeasurement <-
    exportAllPolicies
      [(freeNftPolicyScript, outDir, "free-nft-policy", V2)]

  fixedSupplyMeasurement <-
    exportAllPolicies
      [(fixedSupplyPolicyUnapplied, outDir, "fixed-supply-unapplied-policy", V2)]

  let exportInfo =
        ExportInfo
          { launchpadConstants =
              LaunchpadConstants
                { minNodeIndex = C.minNodeIndex
                , maxNodeIndex = C.maxNodeIndex
                , separatorNodeKeyLength = C.separatorNodeKeyLength
                , nodesInactivityPeriod = C.nodesInactivityPeriod
                , emergencyWithdrawalPeriod = C.emergencyWithdrawalPeriod
                }
          , validators =
              commitFoldMeasurement
                ++ nodeMeasurement
                ++ poolProofMeasurement
                ++ projectTokensHolderFinalMeasurement
                ++ projectTokensHolderFirstMeasurement
                ++ rewardsFoldMeasurement
                ++ rewardsHolderMeasurement
                ++ failProofMeasurement
                ++ refScriptCarrierMeasurement
                ++ vestingMeasurement
          , policies =
              commitFoldTokenMeasurement
                ++ nodeTokenMeasurement
                ++ poolProofTokenMeasurement
                ++ projectTokensHolderTokenMeasurement
                ++ rewardsFoldTokenMeasurement
                ++ failProofTokenMeasurement
                ++ alwaysFailsMeasurement
                ++ freeNftMeasurement
                ++ fixedSupplyMeasurement
          }
  writeToJson exportInfo $ outDir <> "export-info.json"

  putStrLn $ "Exported contracts to: " <> outDir
