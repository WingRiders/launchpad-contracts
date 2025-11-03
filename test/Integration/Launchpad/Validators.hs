module Integration.Launchpad.Validators where

import Integration.Compile (mkTypedPolicyPlutarchV2, mkTypedValidatorPlutarchV2)
import Integration.Mock
import Integration.Util
import Launchpad.CommitFold qualified as C
import Launchpad.FailProof qualified as FP
import Launchpad.Mint.CommitFold qualified as C
import Launchpad.Mint.FailProof qualified as FP
import Launchpad.Mint.Node qualified as N
import Launchpad.Mint.PoolProof qualified as PP
import Launchpad.Mint.ProjectTokensHolder qualified as PTH
import Launchpad.Mint.RewardsFold qualified as R
import Launchpad.Node qualified as N
import Launchpad.PoolProof qualified as PP
import Launchpad.ProjectTokensHolderFinal qualified as PTHF
import Launchpad.ProjectTokensHolderFirst qualified as PTHFirst
import Launchpad.RewardsFold qualified as R
import Launchpad.RewardsHolder qualified as RH
import Launchpad.Types
import Other.FreePolicy (pvalidateFreePolicy)
import Plutarch.Prelude
import Plutus.Model

projectTokensHolderFirstValidator :: LaunchpadConfig -> TypedValidator LaunchpadTokensHolderDatum LaunchpadTokensHolderFirstRedeemer
projectTokensHolderFirstValidator config = case mkTypedValidatorPlutarchV2
  (PTHFirst.projectTokensHolderValidator # pconstant (firstTokensHolderConfig config)) of
  Left err -> error $ show err
  Right val -> val

projectTokensHolderFinalValidator :: LaunchpadConfig -> TypedValidator () PTHF.TokenHolderRedeemerFinal
projectTokensHolderFinalValidator config = case mkTypedValidatorPlutarchV2
  (PTHF.projectTokensHolderValidator # pconstant (finalWrTokensHolderConfig config)) of
  Left err -> error $ show err
  Right val -> val

projectTokensHolderMintingPolicy :: LaunchpadConfig -> TypedPolicy ()
projectTokensHolderMintingPolicy config = case mkTypedPolicyPlutarchV2
  (PTH.pprojectTokensHolderMintingPolicy # pconstant (tokensHolderPolicyConfig config)) of
  Left err -> error $ show err
  Right val -> val

nodeValidator :: LaunchpadConfig -> TypedValidator Node NodeRedeemer
nodeValidator config = case mkTypedValidatorPlutarchV2 (N.nodeValidator # pconstant (nodeConfig config)) of
  Left err -> error $ show err
  Right val -> val

nodeMintingPolicy :: LaunchpadConfig -> TypedPolicy ()
nodeMintingPolicy config = case mkTypedPolicyPlutarchV2 (N.pnodeMintingPolicy # pconstant (nodePolicyConfig config)) of
  Left err -> error $ show err
  Right val -> val

commitFoldValidator :: LaunchpadConfig -> TypedValidator CommitFoldDatum CommitFoldRedeemer
commitFoldValidator config = case mkTypedValidatorPlutarchV2 (C.commitFoldValidator # pconstant (commitFoldConfig config)) of
  Left err -> error $ "commitFoldValidator: " <> show err
  Right val -> val

commitFoldMintingPolicy :: LaunchpadConfig -> TypedPolicy ()
commitFoldMintingPolicy config = case mkTypedPolicyPlutarchV2
  (C.pcommitFoldMintingPolicy # pconstant (commitFoldPolicyConfig config)) of
  Left err -> error $ "commitFoldMintingPolicy: " <> show err
  Right val -> val

poolProofValidator :: LaunchpadConfig -> TypedValidator PoolProofDatum ()
poolProofValidator config = case mkTypedValidatorPlutarchV2 (PP.poolProofValidator # pcon (PP.PPoolProofConfig (pdata (pconstant (poolProofConfig config))))) of
  Left err -> error $ show err
  Right val -> val

poolProofMintingPolicy :: LaunchpadConfig -> TypedPolicy Integer
poolProofMintingPolicy config = case mkTypedPolicyPlutarchV2 (PP.ppoolProofMintingPolicy # pconstant (poolProofPolicyConfig config)) of
  Left err -> error $ show err
  Right val -> val

failProofValidator :: LaunchpadConfig -> TypedValidator FailProofDatum ()
failProofValidator _config = case mkTypedValidatorPlutarchV2 FP.failProofValidator of
  Left err -> error $ show err
  Right val -> val

failProofMintingPolicy :: LaunchpadConfig -> TypedPolicy ()
failProofMintingPolicy _config = case mkTypedPolicyPlutarchV2 FP.pfailProofMintingPolicy of
  Left err -> error $ show err
  Right val -> val

rewardsHolderValidator :: LaunchpadConfig -> TypedValidator RewardsHolderDatum ()
rewardsHolderValidator config = case mkTypedValidatorPlutarchV2 (RH.rewardsHolderValidator # pconstant (rewardsHolderConfig config)) of
  Left err -> error $ show err
  Right val -> val

rewardsFoldValidator :: LaunchpadConfig -> TypedValidator RewardsFoldDatum RewardsFoldRedeemer
rewardsFoldValidator config = case mkTypedValidatorPlutarchV2 (R.rewardsFoldValidator # pconstant (rewardsFoldConfig config)) of
  Left err -> error $ "rewardsFoldValidator: " <> show err
  Right val -> val

rewardsFoldMintingPolicy :: LaunchpadConfig -> TypedPolicy ()
rewardsFoldMintingPolicy config = case mkTypedPolicyPlutarchV2
  (R.prewardsFoldMintingPolicy # pcon (R.PRewardsFoldPolicyConfig (pdata (pconstant (rewardsFoldPolicyConfig config))))) of
  Left err -> error $ "rewardsFoldMintingPolicy: " <> show err
  Right val -> val

poolMintingPolicy :: TypedPolicy ()
poolMintingPolicy = case mkTypedPolicyPlutarchV2 pvalidateFreePolicy of
  Left err -> error $ show err
  Right val -> val

mockFactoryScript :: TypedValidator () ()
mockFactoryScript = case mkTypedValidatorPlutarchV2 (plam $ \_ _ _ -> popaque (pconstant ())) of
  Left err -> error $ show err
  Right val -> val
