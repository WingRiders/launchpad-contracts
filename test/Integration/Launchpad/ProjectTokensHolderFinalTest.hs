module Integration.Launchpad.ProjectTokensHolderFinalTest where

import Integration.Launchpad.PoolProof (createPoolProof, createWrPoolUTxO)
import Integration.Launchpad.PoolProof qualified as PP
import Integration.Launchpad.ProjectTokensHolderFinal (
  MaliciousWrTokenHolderAction (..),
  createProjectTokensHolderFinal,
  spendHolderCreatePool,
  spendHolderPoolExists,
 )
import Integration.Mock
import Integration.Util
import Plutus.Model
import Test.Tasty (
  TestTree,
  testGroup,
 )
import Test.Util (vBTC, vETH)

projectTokensHolderFinalTests :: TestTree
projectTokensHolderFinalTests =
  testGroup
    "ProjectTokensHolderFinal"
    [ testGroup "Spend ProjectTokensHolderFinal - No Pool" spendTestsNoPool
    , testGroup "Spend ProjectTokensHolderFinal - Pool Exists" spendTestsPoolExists
    ]

spendTestsNoPool :: [TestTree]
spendTestsNoPool =
  [ good defaultLaunchpadConfig "Holder can be spent when all conditions are met" (lock_to_pool None)
  , good defaultLaunchpadConfig {raisingToken = vBTC, projectToken = vETH} "Holder can be spent if raisingAssetClass > projectAssetClass hashed correctly" (lock_to_pool None)
  , bad defaultLaunchpadConfig {raisingToken = vBTC, projectToken = vETH} "Fails when raisingAssetClass > projectAssetClass is hashed incorrectly" (lock_to_pool WrongHashOrder)
  , bad defaultLaunchpadConfig "Fails when beneficiary is not set to owner" (lock_to_pool WrongBeneficiary)
  , bad defaultLaunchpadConfig "Fails when less tokens are locked than set in datum" (lock_to_pool WrongVestingQuantity)
  , bad defaultLaunchpadConfig "Fails when vesting start is not current upper time approximation" (lock_to_pool WrongPeriodStart)
  , bad defaultLaunchpadConfig "Fails when vesting end does not match config" (lock_to_pool WrongPeriodEnd)
  , bad defaultLaunchpadConfig "Fails when vesting 1st unlock does not match config" (lock_to_pool WrongFirstUnlock)
  , bad defaultLaunchpadConfig "Fails when vesting installments do not match config" (lock_to_pool WrongInstallments)
  , bad defaultLaunchpadConfig "Fails when vesting asset does not match locked asset" (lock_to_pool WrongVestingAsset)
  , bad defaultLaunchpadConfig "Fails when there are more than 2 token types locked inside vesting" (lock_to_pool MultipleTokenTypes)
  , bad defaultLaunchpadConfig "Fails when less fees are paid to daoFeeReceiver" (lock_to_pool LessDaoFees)
  , bad defaultLaunchpadConfig "Fails when no fees are paid to daoFeeReceiver" (lock_to_pool NoDaoFees)
  , bad defaultLaunchpadConfig "Fails when launchpad owner is not compensated" (lock_to_pool NoOwnerCompensations)
  , bad defaultLaunchpadConfig "Fails when token holders is doublespent and dao fees stolen" double_spend_two_token_holders
  , bad defaultLaunchpadConfig "Fails when the tokens holder validity token is not burned" (lock_to_pool NoBurnedHolderToken)
  ]

lock_to_pool :: MaliciousWrTokenHolderAction -> LaunchpadConfig -> Run ()
lock_to_pool action config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  createProjectTokensHolderFinal config 1_000_000 adminWallet
  spendHolderCreatePool action config adminWallet launchpadOwner

spendTestsPoolExists :: [TestTree]
spendTestsPoolExists =
  [ good defaultLaunchpadConfig "Holder can be spent when all conditions are met" (move_funds_to_dao None)
  , bad defaultLaunchpadConfig "Fails when less fees are paid to daoFeeReceiver" (move_funds_to_dao LessDaoFees)
  , bad defaultLaunchpadConfig "Fails when no fees are paid to daoFeeReceiver" (move_funds_to_dao NoDaoFees)
  , bad defaultLaunchpadConfig "Fails when no pool proof is present" (move_funds_to_dao NoPoolProof)
  , bad defaultLaunchpadConfig "Fails when less project tokens are given to dao for locking" (move_funds_to_dao LessProjectTokensToDao)
  , bad defaultLaunchpadConfig "Fails when no project tokens are given to dao for locking" (move_funds_to_dao NoProjectTokensToDao)
  , bad defaultLaunchpadConfig "Fails when launchpad owner is not compensated" (move_funds_to_dao NoOwnerCompensations)
  , bad defaultLaunchpadConfig "Fails when wrong pool proof is present" (move_funds_to_dao WrongPoolProof)
  , bad defaultLaunchpadConfig "Fails when the tokens holder validity token is not burned" (move_funds_to_dao NoBurnedHolderToken)
  ]

move_funds_to_dao :: MaliciousWrTokenHolderAction -> LaunchpadConfig -> Run ()
move_funds_to_dao action config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  let poolConfig = case action of
        WrongPoolProof -> config {projectToken = vETH}
        _ -> config

  createWrPoolUTxO PP.None poolConfig poolWrInitWallet
  createPoolProof PP.None poolConfig userWallet2
  createProjectTokensHolderFinal config 1_000_000 adminWallet
  spendHolderPoolExists action config adminWallet launchpadOwner

double_spend_two_token_holders :: LaunchpadConfig -> Run ()
double_spend_two_token_holders config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  createProjectTokensHolderFinal config 1_000_000 adminWallet
  createProjectTokensHolderFinal config 1_000_000 adminWallet

  spendHolderCreatePool DoubleSatisfy config adminWallet launchpadOwner
