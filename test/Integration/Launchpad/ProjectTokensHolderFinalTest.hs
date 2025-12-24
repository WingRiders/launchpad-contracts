module Integration.Launchpad.ProjectTokensHolderFinalTest where

import Control.Monad (when)
import Integration.Launchpad.PoolProof (createPoolProof, createPoolUtxo)
import Integration.Launchpad.PoolProof qualified as PP
import Integration.Launchpad.ProjectTokensHolderFinal (
  MaliciousTokensHolderAction (..),
  createProjectTokensHolderFinal,
  spendHolderCreatePool,
  spendHolderFundsToDao,
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
    [ testGroup "Spend ProjectTokensHolderFinal - Wr - No Pool" spendTestsWrNoPool
    , testGroup "Spend ProjectTokensHolderFinal - Wr - Pool Exists" spendTestsWrPoolExists
    , testGroup "Spend ProjectTokensHolderFinal - Sundae" spendTestsSundae
    ]

spendTestsWrNoPool :: [TestTree]
spendTestsWrNoPool =
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

spendTestsSundae :: [TestTree]
spendTestsSundae =
  [ good defaultLaunchpadConfig {splitBps = 0} "Holder can be spent when all conditions are met" (lock_to_pool None)
  , good defaultLaunchpadConfig {splitBps = 0, sundaeFee = 10} "Holder can be spent when all conditions are met and pool creation fee is below the tolerance" (lock_to_pool None)
  , bad defaultLaunchpadConfig {splitBps = 0, sundaeFee = 10} "DAO can't get the funds if the fee is below torelance" (move_funds_to_dao None)
  , good defaultLaunchpadConfig {splitBps = 0, sundaeFee = defaultLaunchpadConfig.sundaeFeeTolerance + 1} "DAO gets the funds if the fee is above torelance" (move_funds_to_dao None)
  ]

lock_to_pool :: MaliciousTokensHolderAction -> LaunchpadConfig -> Run ()
lock_to_pool action config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  when (config.splitBps > 0) $ do
    createProjectTokensHolderFinal config Wr 1_000_000 adminWallet
    spendHolderCreatePool action Wr config adminWallet launchpadOwner

  when (config.splitBps < 10_000) $ do
    createProjectTokensHolderFinal config Sundae 1_000_000 adminWallet
    spendHolderCreatePool action Sundae config adminWallet launchpadOwner

spendTestsWrPoolExists :: [TestTree]
spendTestsWrPoolExists =
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
    spendHolderFundsToDao action config Wr adminWallet launchpadOwner

  when (config.splitBps < 10_000) $ do
    createProjectTokensHolderFinal config Sundae 1_000_000 adminWallet
    spendHolderFundsToDao action config Sundae adminWallet launchpadOwner

double_spend_two_token_holders :: LaunchpadConfig -> Run ()
double_spend_two_token_holders config = do
  Wallets {..} <- setupWallets config
  adminWallet <- getMainUser

  createProjectTokensHolderFinal config Wr 1_000_000 adminWallet
  createProjectTokensHolderFinal config Wr 1_000_000 adminWallet

  spendHolderCreatePool DoubleSatisfy Wr config adminWallet launchpadOwner
