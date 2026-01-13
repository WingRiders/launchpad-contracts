{-# LANGUAGE BlockArguments #-}

module Integration.Launchpad.RewardsHolderTest where

import Control.Monad (when)
import Integration.Launchpad.PoolProof
import Integration.Launchpad.PoolProof qualified as PP
import Integration.Launchpad.RewardsHolder (MaliciousRewardsHolderAction (..), createRewardsHolder, spendRewardsHolder)
import Integration.Launchpad.RewardsHolder qualified as RH
import Integration.Mock
import Integration.Util
import Launchpad.Types (Dex (..))
import Plutus.Model
import PlutusLedgerApi.V1.Value (assetClassValue)
import PlutusLedgerApi.V2 (Value)
import Test.Tasty (
  TestTree,
  testGroup,
 )

rewardsHolderTests :: TestTree
rewardsHolderTests =
  testGroup
    "Rewards Holder"
    [ testGroup "Spend RewardsHolder" spendTests
    ]

defaultRewardsValue :: Value
defaultRewardsValue = assetClassValue defaultLaunchpadConfig.projectToken 100

spendTests :: [TestTree]
spendTests =
  [ good defaultLaunchpadConfig "Owner can spend their RewardsHolder" (spend_rewards_holder RH.None 1 defaultRewardsValue)
  , good defaultLaunchpadConfig "Owner can spend multiple RewardHolders in one TX" (spend_rewards_holder RH.None 3 defaultRewardsValue)
  , bad defaultLaunchpadConfig "Owner can't spend their RewardsHolder without PoolProof" (spend_rewards_holder NoPoolProof 1 defaultRewardsValue)
  , bad defaultLaunchpadConfig "Owner can't spend their RewardsHolder with PoolProof of different pool" (spend_rewards_holder WrongPoolProof 1 defaultRewardsValue)
  , bad defaultLaunchpadConfig "User can't spend someone elses RewardsHolder" (spend_rewards_holder NoOwnerSignature 1 defaultRewardsValue)
  ]

spend_rewards_holder :: MaliciousRewardsHolderAction -> Integer -> Value -> LaunchpadConfig -> Run ()
spend_rewards_holder action n val config = do
  Wallets {..} <- setupWallets config

  when (config.splitBps > 0) do
    createPoolUtxo PP.None Wr config poolInitWallet
    createPoolProof PP.None Wr config userWallet2

  when (config.splitBps < 10_000) do
    createPoolUtxo PP.None Sundae config poolInitWallet
    createPoolProof PP.None Sundae config userWallet2

  mapM_ (\i -> createRewardsHolder action config val (unwrapPubKeyHash userWallet1, i) launchpadOwner) [1 .. n]
  spendRewardsHolder action config userWallet1
