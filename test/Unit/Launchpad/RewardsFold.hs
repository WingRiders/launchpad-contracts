{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Unit.Launchpad.RewardsFold (rewardsFoldTests) where

import Integration.Mock (nodeAdaAmount, rewardsHolderOilAdaAmount)
import Launchpad.RewardsFold
import Launchpad.Types
import Plutarch.Api.V1.Value (passertPositive, passertSorted)
import Plutarch.Api.V2 (PTxOut)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Util hiding (passertPositive)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V2 (
  Address,
  CurrencySymbol,
  Datum (..),
  OutputDatum (..),
  PubKeyHash,
  ScriptHash,
  ToData (..),
  TokenName (..),
  TxOut (..),
  Value,
  adaSymbol,
  adaToken,
  singleton,
 )
import Test.Tasty
import Test.Tasty.HUnit
import Unit.Launchpad.UtilFunctions (unwrapPubKeyHash, unwrapScriptHash)

rewardsFoldTests :: TestTree
rewardsFoldTests =
  testGroup
    "RewardsFold"
    [ pnextRewardsStateTests
    , pdetermineRewardsTests
    , pcountOfUniqueTokensWithOverlapTests
    ]

mockProjectSymbol :: CurrencySymbol
mockProjectSymbol = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c0"

mockProjectToken :: TokenName
mockProjectToken = "Project"

usdcSymbol :: CurrencySymbol
usdcSymbol = "aa614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb0000"

usdcToken :: TokenName
usdcToken = "USDC"

mockNodeHash :: ScriptHash
mockNodeHash = "9876543210"

mockNodeCs :: CurrencySymbol
mockNodeCs = "12345aaaa67890"

mockRewardsHolderHash :: ScriptHash
mockRewardsHolderHash = "1234567890"

mockNextPkh :: PubKeyHash
mockNextPkh = "01614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb0000"

mockCutoffPkh :: PubKeyHash
mockCutoffPkh = "00614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb0000"

mockCommitFoldOwner :: Address
mockCommitFoldOwner = pubKeyHashAddress mockCutoffPkh

mockPresaleCs :: CurrencySymbol
mockPresaleCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb0000"

mockPresaleTn :: TokenName
mockPresaleTn = "presale"

pcountOfUniqueTokensWithOverlapTests :: TestTree
pcountOfUniqueTokensWithOverlapTests =
  testGroup
    "pcountOfUniqueTokensWithOverlap"
    [ testCase "returns 4 on value with ada, project tokens, committed stablecoins, and a tier token" $
        pcountOfUniqueTokensWithOverlap
          (pconstant usdcSymbol)
          ( passertPositive
              #$ passertSorted
              # ( pconstant
                    ( adaValue 10_000_000
                        <> usdcValue 2_000
                        <> singleton mockPresaleCs mockPresaleTn 1
                        <> singleton mockProjectSymbol mockProjectToken 2_000
                    )
                )
          )
          #@?= pconstant @PInteger 4
    , testCase "returns 4 on value with committed ada, project tokens, and a tier token" $
        pcountOfUniqueTokensWithOverlap
          (pconstant adaSymbol)
          ( passertPositive
              #$ passertSorted
              # ( pconstant
                    ( adaValue 10_000_000
                        <> singleton mockPresaleCs mockPresaleTn 1
                        <> singleton mockProjectSymbol mockProjectToken 2_000
                    )
                )
          )
          #@?= pconstant @PInteger 4
    , testCase "returns 3 on value with ada, project tokens, and committed stablecoins" $
        pcountOfUniqueTokensWithOverlap
          (pconstant usdcSymbol)
          ( passertPositive
              #$ passertSorted
              # ( pconstant
                    ( adaValue 10_000_000
                        <> usdcValue 2_000
                        <> singleton mockProjectSymbol mockProjectToken 2_000
                    )
                )
          )
          #@?= pconstant @PInteger 3
    , testCase "returns 3 on value with committed ada, and project tokens" $
        pcountOfUniqueTokensWithOverlap
          (pconstant adaSymbol)
          ( passertPositive
              #$ passertSorted
              # ( pconstant
                    ( adaValue 10_000_000
                        <> singleton mockProjectSymbol mockProjectToken 2_000
                    )
                )
          )
          #@?= pconstant @PInteger 3
    ]

pdetermineRewardsTests :: TestTree
pdetermineRewardsTests =
  testGroup
    "pdetermineRewards"
    [ testCase "returns 0 project tokens, returns all committed tokens and takes 0 committed tokens for a time-ineligible node" $
        ( pletFields
            @'["next", "key", "committed", "createdTime"]
            (pconstant (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_346 2_000))
            ( pdetermineRewards
                (pdjust (pdata 12_345))
                (pdjust (pdata $ pbuiltinPair (pto (pconstant mockCutoffPkh)) 0))
                1_000
                5_000
                100_000
            )
        )
          #@?= ( pcon
                  PCalculatedRewards
                    { projectTokens = 0
                    , returnedCommittedTokens = 2_000
                    , takenCommittedTokens = 0
                    }
               )
    , testCase "returns 0 project tokens and returns all committed tokens for a key-ineligible node" $
        ( pletFields
            @'["next", "key", "committed", "createdTime"]
            (pconstant (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_345 2_000))
            ( pdetermineRewards
                (pdjust (pdata 12_345))
                (pdjust (pdata $ pbuiltinPair (pto (pconstant mockCutoffPkh)) 0))
                1_000
                5_000
                100_000
            )
        )
          #@?= ( pcon
                  PCalculatedRewards
                    { projectTokens = 0
                    , returnedCommittedTokens = 2_000
                    , takenCommittedTokens = 0
                    }
               )
    , testCase "returns the expected number of project tokens and takes all committed tokens for a fully time-eligible node" $
        ( pletFields
            @'["next", "key", "committed", "createdTime"]
            (pconstant (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000))
            ( pdetermineRewards
                (pdjust (pdata 12_345))
                (pdjust (pdata $ pbuiltinPair (pto (pconstant mockCutoffPkh)) 0))
                1_000
                5_000
                100_000
            )
        )
          #@?= ( pcon
                  PCalculatedRewards
                    { projectTokens = prewardsFormula # 5_000 # 100_000 # 2_000
                    , returnedCommittedTokens = 0
                    , takenCommittedTokens = 2_000
                    }
               )
    , testCase "returns the expected number of project tokens and takes all committed for a fully key-eligible node" $
        ( pletFields
            @'["next", "key", "committed", "createdTime"]
            (pconstant (Node (Just (unwrapPubKeyHash mockCutoffPkh, 0)) Nothing 12_345 2_000))
            ( pdetermineRewards
                (pdjust (pdata 12_345))
                (pdjust (pdata $ pbuiltinPair (pto (pconstant mockNextPkh)) 0))
                1_000
                5_000
                100_000
            )
        )
          #@?= ( pcon
                  PCalculatedRewards
                    { projectTokens = prewardsFormula # 5_000 # 100_000 # 2_000
                    , returnedCommittedTokens = 0
                    , takenCommittedTokens = 2_000
                    }
               )
    , testCase "returns the expected number of project tokens and the overcommitted number of committed tokens, and takes the remaining committed tokens for the cutoff node" $
        ( pletFields
            @'["next", "key", "committed", "createdTime"]
            (pconstant (Node (Just (unwrapPubKeyHash mockCutoffPkh, 0)) Nothing 12_345 2_000))
            ( pdetermineRewards
                (pdjust (pdata 12_345))
                (pdjust (pdata $ pbuiltinPair (pto (pconstant mockCutoffPkh)) 0))
                1_000
                5_000
                100_000
            )
        )
          #@?= ( pcon
                  PCalculatedRewards
                    { projectTokens = prewardsFormula # 5_000 # 100_000 # (2_000 - 1_000)
                    , returnedCommittedTokens = 1_000
                    , takenCommittedTokens = 2_000 - 1_000
                    }
               )
    ]

outDatum :: ToData a => a -> OutputDatum
outDatum a = OutputDatum $ Datum $ toBuiltinData a

outNode :: Value -> Node -> ClosedTerm PTxOut
outNode value node = pconstant (TxOut (scriptHashAddress mockNodeHash) value (outDatum node) Nothing)

outRewardWithAddress :: Address -> Value -> RewardsHolderDatum -> ClosedTerm PTxOut
outRewardWithAddress address value datum = pconstant (TxOut address value (outDatum datum) Nothing)

outReward :: Value -> RewardsHolderDatum -> ClosedTerm PTxOut
outReward = outRewardWithAddress (scriptHashAddress mockRewardsHolderHash)

adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken

usdcValue :: Integer -> Value
usdcValue = singleton usdcSymbol usdcToken

pnextRewardsStateTests :: TestTree
pnextRewardsStateTests =
  let pnextState = plam $ \state node reward ->
        pnextRewardsStateFirstCome
          (pconstant mockPresaleCs)
          100_000
          (pconstant usdcSymbol)
          (pconstant usdcToken)
          (pconstant mockProjectSymbol)
          (pconstant mockProjectToken)
          (pconstant mockNodeHash)
          (pconstant mockRewardsHolderHash)
          (pconstant rewardsHolderOilAdaAmount)
          # state
          # node
          # reward
      rewardsFold =
        PRewardsFoldScott
          { nodeScriptHash = pconstant mockNodeHash
          , next = pdjust (pdata $ pbuiltinPair (pto (pconstant mockNextPkh)) 0)
          , cutoffKey = pdjust (pdata $ pbuiltinPair (pto (pconstant mockCutoffPkh)) 0)
          , cutoffTime = pdjust (pdata 12_345)
          , committed = 50_000
          , overcommitted = 1_000
          , commitFoldOwner = pconstant mockCommitFoldOwner
          }
      accumulator foldState =
        PRewardsFoldAccumulator
          { nodeCount = 0
          , committedPerTx = 0
          , distributedPerTx = 0
          , foldState = foldState
          }
   in testGroup
        "pnextRewardsStateFirstCome"
        [ testCase "works correctly on ineligible nodes with a full refund reward" $
            ( pnextState
                # pcon (accumulator (pcon rewardsFold))
                # outNode
                  ( adaValue nodeAdaAmount
                      <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                      <> usdcValue 2_000
                  )
                  (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_345 2_000)
                # pjust
                  ( outReward
                      ( adaValue rewardsHolderOilAdaAmount
                          <> usdcValue 2_000
                      )
                      ( RewardsHolderDatum
                          (unwrapPubKeyHash mockNextPkh, 0)
                          mockProjectSymbol
                          mockProjectToken
                          usdcSymbol
                          usdcToken
                      )
                  )
            )
              #@?= ( pcon @(PRewardsFoldAccumulator)
                      (accumulator (pcon @PRewardsFoldScott rewardsFold {next = pdnothing}))
                        { nodeCount = 1
                        }
                   )
        , testCase "works correctly on eligible nodes with correct project tokens rewards" $
            ( pnextState
                # pcon (accumulator (pcon rewardsFold))
                # ( outNode
                      ( adaValue nodeAdaAmount
                          <> usdcValue 2_000
                          <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                      )
                      (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                  )
                # pjust
                  ( outReward
                      ( adaValue rewardsHolderOilAdaAmount
                          <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # 2000))
                      )
                      ( RewardsHolderDatum
                          (unwrapPubKeyHash mockNextPkh, 0)
                          mockProjectSymbol
                          mockProjectToken
                          usdcSymbol
                          usdcToken
                      )
                  )
            )
              #@?= ( pcon @(PRewardsFoldAccumulator)
                      (accumulator (pcon @PRewardsFoldScott rewardsFold {next = pdnothing}))
                        { nodeCount = 1
                        , committedPerTx = 2_000
                        , distributedPerTx = prewardsFormula # 100_000 # 50_000 # 2000
                        }
                   )
        , testCase "works correctly on the cutoff node with the correctly returned rewards" $
            ( pnextState
                # pcon
                  ( accumulator
                      ( pcon @PRewardsFoldScott
                          rewardsFold
                            { next = pdjust (pdata $ pbuiltinPair (pto (pconstant mockCutoffPkh)) 0)
                            }
                      )
                  )
                # ( outNode
                      ( adaValue nodeAdaAmount
                          <> usdcValue 2_000
                          <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                      )
                      (Node (Just (unwrapPubKeyHash mockCutoffPkh, 0)) Nothing 12_345 2_000)
                  )
                # pjust
                  ( outReward
                      ( adaValue rewardsHolderOilAdaAmount
                          <> usdcValue 1_000
                          <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # (2_000 - 1_000)))
                      )
                      ( RewardsHolderDatum
                          (unwrapPubKeyHash mockCutoffPkh, 0)
                          mockProjectSymbol
                          mockProjectToken
                          usdcSymbol
                          usdcToken
                      )
                  )
            )
              #@?= ( pcon @(PRewardsFoldAccumulator)
                      (accumulator (pcon @PRewardsFoldScott rewardsFold {next = pdnothing}))
                        { nodeCount = 1
                        , committedPerTx = 2_000 - 1_000
                        , distributedPerTx = prewardsFormula # 100_000 # 50_000 # (2_000 - 1_000)
                        }
                   )
        , testCase "works correctly on separator nodes with 0 committed tokens and no returned rewards" $
            ( pnextState
                # pcon (accumulator (pcon rewardsFold))
                # ( outNode
                      ( adaValue nodeAdaAmount
                          <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                      )
                      (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 0)
                  )
                # pnothing
            )
              #@?= ( pcon @(PRewardsFoldAccumulator)
                      (accumulator (pcon @PRewardsFoldScott rewardsFold {next = pdnothing}))
                        { nodeCount = 1
                        }
                   )
        , testCase "fails on non-separator nodes when not provided with a rewards output" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                    )
                  # pnothing
              )
        , testCase "fails on separator nodes when provided with a rewards output" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 0)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> usdcValue 1_000
                            <> singleton mockProjectSymbol mockProjectToken 1_000
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails on eligible nodes getting less rewards then they deserve" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> singleton mockProjectSymbol mockProjectToken 1
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails on ineligible nodes getting less committed tokens then they committed" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_346 2_000)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> usdcValue 1_999
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails on rewards using a wrong address" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                    )
                  # pjust
                    ( outRewardWithAddress
                        (pubKeyHashAddress mockNextPkh)
                        ( adaValue rewardsHolderOilAdaAmount
                            <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # 2_000))
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails on rewards with a wrong key stored in the datum" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # 2_000))
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockCutoffPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails on node with a key not equal to the next expected" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 1)) Nothing 12_344 2_000)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # 2_000))
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 1)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "succeeds on nodes with a presale tier token which is returned in the reward" $
            psucceeds
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                            <> singleton mockPresaleCs mockPresaleTn 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # 2_000))
                            <> singleton mockPresaleCs mockPresaleTn 1
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails when the presale token is not returned back" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> usdcValue 2_000
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                            <> singleton mockPresaleCs mockPresaleTn 1
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_344 2_000)
                    )
                  # pjust
                    ( outReward
                        ( adaValue rewardsHolderOilAdaAmount
                            <> singleton mockProjectSymbol mockProjectToken (plift (prewardsFormula # 100_000 # 50_000 # 2_000))
                        )
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            usdcSymbol
                            usdcToken
                        )
                    )
              )
        , testCase "fails when the assets stored in the rewards differ from the ones used by the project" $
            pfails
              ( pnextState
                  # pcon (accumulator (pcon rewardsFold))
                  # ( outNode
                        ( adaValue nodeAdaAmount
                            <> singleton mockNodeCs (TokenName (unwrapScriptHash mockNodeHash)) 1
                            <> usdcValue 2_000
                        )
                        (Node (Just (unwrapPubKeyHash mockNextPkh, 0)) Nothing 12_345 2_000)
                    )
                  # pjust
                    ( outReward
                        (adaValue rewardsHolderOilAdaAmount <> usdcValue 2_000)
                        ( RewardsHolderDatum
                            (unwrapPubKeyHash mockNextPkh, 0)
                            mockProjectSymbol
                            mockProjectToken
                            adaSymbol
                            adaToken
                        )
                    )
              )
        ]
