module Integration.Launchpad.PoolProofTest where

import Integration.Launchpad.PoolProof
import Integration.Mock
import Integration.Util
import Launchpad.Types (Dex (..))
import Plutus.Model
import Test.Tasty (
  TestTree,
  testGroup,
 )
import Test.Util (vUSDT)

poolProofTests :: TestTree
poolProofTests =
  testGroup
    "PoolProof"
    [ testGroup "Create PoolProof" createTests
    , testGroup "Spend PoolProof" spendTests
    ]

createTests :: [TestTree]
createTests =
  [ good defaultLaunchpadConfig "User can create PoolProof Ada Pool" (create_pool_proof None)
  , good defaultLaunchpadConfig {raisingToken = vUSDT} "User can create PoolProof Non-Ada Pool" (create_pool_proof None)
  , bad
      defaultLaunchpadConfig
      "Wrong ScriptHash TokenName results in error"
      (create_pool_proof WrongTokenName)
  , bad
      defaultLaunchpadConfig
      "Minting multiple ScriptHash Tokens results in error"
      (create_pool_proof MintedTwoTokens)
  , bad
      defaultLaunchpadConfig
      "Minting multiple ScriptHash TokenNames results in error"
      (create_pool_proof MintedDifferentTokens)
  , bad
      defaultLaunchpadConfig
      "Different pool ScriptHash in refInputs results in error"
      (create_pool_proof IncorrectPoolHash)
  , bad
      defaultLaunchpadConfig
      "Incorrect pool pair results in error"
      (create_pool_proof DifferentProjectToken)
  ]

spendTests :: [TestTree]
spendTests =
  [ bad defaultLaunchpadConfig "PoolProof can't be spent" create_and_spend_pool_proof
  ]

create_pool_proof :: MaliciousPoolProofAction -> LaunchpadConfig -> Run ()
create_pool_proof action config = do
  Wallets {..} <- setupWallets config
  createPoolUtxo action Wr config poolInitWallet
  createPoolProof action Wr config userWallet2

create_and_spend_pool_proof :: LaunchpadConfig -> Run ()
create_and_spend_pool_proof config = do
  Wallets {..} <- setupWallets config
  createPoolUtxo None Wr config poolInitWallet
  createPoolProof None Wr config userWallet1
  spendPoolProof config userWallet1
