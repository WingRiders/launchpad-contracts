module Integration.Launchpad.ProjectTokensHolderFinalTest where

import Control.Monad (when)
import Integration.Launchpad.PoolProof (createPoolProof, createPoolUtxo)
import Integration.Launchpad.PoolProof qualified as PP
import Integration.Launchpad.ProjectTokensHolderFinal (
  MaliciousTokensHolderAction (..),
  createProjectTokensHolderFinal,
  spendHolderCreatePool,
  spendHolderPoolExists,
 )
import Integration.Mock
import Integration.Util
import Launchpad.Types (Dex (..))
import Plutus.Model
import Test.Tasty (
  TestTree,
  testGroup,
 )
import Test.Util (vETH)

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
  , bad defaultLaunchpadConfig "Fails when beneficiary is not set to owner" (lock_to_pool WrongBeneficiary)
  , bad defaultLaunchpadConfig "Fails when less tokens are locked than set in datum" (lock_to_pool WrongVestingQuantity)
  , bad defaultLaunchpadConfig "Fails when vesting start is not current upper time approximation" (lock_to_pool WrongPeriodStart)
  , bad defaultLaunchpadConfig "Fails when vesting end does not match config" (lock_to_pool WrongPeriodEnd)
  , bad defaultLaunchpadConfig "Fails when vesting 1st unlock does not match config" (lock_to_pool WrongFirstUnlock)
  , bad defaultLaunchpadConfig "Fails when vesting installments do not match config" (lock_to_pool WrongInstallments)
  , bad defaultLaunchpadConfig "Fails when vesting asset does not match locked asset" (lock_to_pool WrongVestingAsset)
  , bad defaultLaunchpadConfig "Fails when there are more than 2 token types locked inside vesting" (lock_to_pool MultipleTokenTypes)
  , bad defaultLaunchpadConfig "Fails when token holders are doublespent" double_spend_two_token_holders
  ]

lock_to_pool :: MaliciousTokensHolderAction -> LaunchpadConfig -> Run ()
lock_to_pool action config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  createProjectTokensHolderFinal config Wr 1_000_000 adminWallet
  spendHolderCreatePool action Wr config adminWallet launchpadOwner

spendTestsPoolExists :: [TestTree]
spendTestsPoolExists =
  [ good defaultLaunchpadConfig "Holder can be spent when all conditions are met" (move_funds_to_dao None)
  , bad defaultLaunchpadConfig "Fails when no pool proof is present" (move_funds_to_dao NoPoolProof)
  , bad defaultLaunchpadConfig "Fails when less project tokens are given to dao for locking" (move_funds_to_dao LessProjectTokensToDao)
  , bad defaultLaunchpadConfig "Fails when no project tokens are given to dao for locking" (move_funds_to_dao NoProjectTokensToDao)
  , bad defaultLaunchpadConfig "Fails when wrong pool proof is present" (move_funds_to_dao WrongPoolProof)
  ]

move_funds_to_dao :: MaliciousTokensHolderAction -> LaunchpadConfig -> Run ()
move_funds_to_dao action config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  let poolConfig = case action of
        WrongPoolProof -> config {projectToken = vETH}
        _ -> config

  when (config.splitBps > 0) $ do
    createPoolUtxo PP.None Wr poolConfig poolInitWallet
    createPoolProof PP.None Wr poolConfig userWallet2
    createProjectTokensHolderFinal config Wr 1_000_000 adminWallet
    spendHolderPoolExists action config adminWallet launchpadOwner

double_spend_two_token_holders :: LaunchpadConfig -> Run ()
double_spend_two_token_holders config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  createProjectTokensHolderFinal config Wr 1_000_000 adminWallet
  createProjectTokensHolderFinal config Wr 1_000_000 adminWallet

  spendHolderCreatePool DoubleSatisfy Wr config adminWallet launchpadOwner
