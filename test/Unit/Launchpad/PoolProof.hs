module Unit.Launchpad.PoolProof where

import Launchpad.Util (pisCorrectPool)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Util hiding (passertPositive)
import PlutusLedgerApi.V1.Value
import Test.Tasty
import Test.Tasty.HUnit

poolProofMintingPolicyTests :: TestTree
poolProofMintingPolicyTests =
  testGroup
    "PoolProof Minting Policy Tests"
    [ pisCorrectPoolTests
    ]

mockProjectTokenCs :: CurrencySymbol
mockProjectTokenCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c0"

mockProjectTokenTn :: TokenName
mockProjectTokenTn = "mockproject"

mockProjectToken :: AssetClass
mockProjectToken = assetClass mockProjectTokenCs mockProjectTokenTn

mockRaisingTokenCs :: CurrencySymbol
mockRaisingTokenCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c1"

mockRaisingTokenTn :: TokenName
mockRaisingTokenTn = "mockraising"

mockRaisingToken :: AssetClass
mockRaisingToken = assetClass mockRaisingTokenCs mockRaisingTokenTn

pisCorrectPoolTests :: TestTree
pisCorrectPoolTests =
  testGroup
    "pisCorrectPool"
    [ testCase "true on correct LP (ada pool)" $
        pisCorrectPool
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          (pconstant adaSymbol, pconstant adaToken)
          (pconstant adaSymbol, pconstant adaToken)
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          #@?= ptrue
    , testCase "true on correct LP (non-ada pool)" $
        pisCorrectPool
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          (pconstant mockRaisingTokenCs, pconstant mockRaisingTokenTn)
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          (pconstant mockRaisingTokenCs, pconstant mockRaisingTokenTn)
          #@?= ptrue
    , testCase "false on incorrect LP (ada pool)" $
        pisCorrectPool
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          (pconstant mockRaisingTokenCs, pconstant mockRaisingTokenTn)
          (pconstant adaSymbol, pconstant adaToken)
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          #@?= pfalse
    , testCase "false on incorrect LP (non-ada pool)" $
        pisCorrectPool
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          (pconstant adaSymbol, pconstant adaToken)
          (pconstant mockProjectTokenCs, pconstant mockProjectTokenTn)
          (pconstant mockRaisingTokenCs, pconstant mockRaisingTokenTn)
          #@?= pfalse
    ]
