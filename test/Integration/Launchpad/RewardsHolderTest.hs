module Integration.Launchpad.RewardsHolderTest where

import Integration.Launchpad.PoolProof
import Integration.Launchpad.PoolProof qualified as PP
import Integration.Launchpad.RewardsHolder (MaliciousRewardsHolderAction (..), createRewardsHolder, spendRewardsHolder)
import Integration.Launchpad.RewardsHolder qualified as RH
import Integration.Mock
import Integration.Util
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

  createWrPoolUTxO PP.None config poolWrInitWallet
  createPoolProof PP.None config userWallet2
  mapM_ (\i -> createRewardsHolder action config val (unwrapPubKeyHash userWallet1, i) launchpadOwner) [1 .. n]
  spendRewardsHolder action config userWallet1
