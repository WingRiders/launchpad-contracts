{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Unit.Launchpad.CommitFold (commitFoldTests) where

import Integration.Mock (nodeAdaAmount)
import Launchpad.CommitFold
import Launchpad.Types
import Plutarch.Api.V1.Value (passertPositive, passertSorted)
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Util hiding (passertPositive)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V2
import Test.Tasty
import Test.Tasty.HUnit
import Unit.Launchpad.UtilFunctions (unwrapPubKeyHash, unwrapScriptHash)

commitFoldTests :: TestTree
commitFoldTests = testGroup "CommitFold" [pnextCommitStateFirstComeTests, pisCommitFoldValueCorrectTests]

mockCs :: CurrencySymbol
mockCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c0"

mockCs2 :: CurrencySymbol
mockCs2 = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c1"

mockTn :: TokenName
mockTn = "mock1"

mockTn2 :: TokenName
mockTn2 = "mock2"

mockPubKeyHash0 :: PubKeyHash
mockPubKeyHash0 = "12345678"

mockPubKeyHash1 :: PubKeyHash
mockPubKeyHash1 = "23456789"

mockPubKeyHash2 :: PubKeyHash
mockPubKeyHash2 = "34567890"

mockScriptHash1 :: ScriptHash
mockScriptHash1 = "98765432"

mockScriptHash2 :: ScriptHash
mockScriptHash2 = "87654321"

usdcSymbol = "0123"
usdcToken = "USDC"
usdcAssetClass = AssetClass (usdcSymbol, usdcToken)

adaValue = singleton adaSymbol adaToken
usdcValue = singleton usdcSymbol usdcToken

initialFirstCome :: Term s PCommitFoldScott
initialFirstCome =
  pcon
    ( PCommitFoldScott
        { nodeScriptHash = pconstant mockScriptHash1
        , next = pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash1)) 0)
        , committed = 0
        , cutoffKey = pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0)
        , cutoffTime = pdjust (pdata $ pconstant 12_346)
        , overcommitted = 100
        , nodeCount = 1
        , owner = pconstant (pubKeyHashAddress mockPubKeyHash1)
        }
    )

asOutputDatum :: ToData p => p -> OutputDatum
asOutputDatum a = OutputDatum . Datum . toBuiltinData $ a

outNode :: Maybe NodeKey -> Maybe NodeKey -> POSIXTime -> Integer -> OutputDatum
outNode key next createdTime committed = asOutputDatum $ Node key next createdTime committed

pnextCommitStateFirstComeTests :: TestTree
pnextCommitStateFirstComeTests =
  let nextState = pnextCommitState # pconstant mockCs2 # initialFirstCome
   in testGroup
        "pnextCommitStateFirstCome"
        [ testCase "the next expected fully eligible (by time) node contributes the full commitment" $
            ( nextState
                # pconstant
                  ( TxOut
                      (scriptHashAddress mockScriptHash1)
                      ( adaValue nodeAdaAmount
                          <> usdcValue 1_000
                          <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                      )
                      (outNode (Just (unwrapPubKeyHash mockPubKeyHash1, 0)) Nothing 12_345 1_000)
                      Nothing
                  )
            )
              #@?= ( pcon
                      ( PCommitFoldScott
                          (pconstant mockScriptHash1)
                          pdnothing
                          1_000
                          (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                          (pdjust (pdata $ pconstant 12_346))
                          100
                          2
                          (pconstant (pubKeyHashAddress mockPubKeyHash1))
                      )
                   )
        , testCase "the next expected eligible node (by key) contributes the full commitment" $
            ( nextState
                # pconstant
                  ( TxOut
                      (scriptHashAddress mockScriptHash1)
                      ( adaValue nodeAdaAmount
                          <> usdcValue 1_000
                          <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                      )
                      (outNode (Just (unwrapPubKeyHash mockPubKeyHash1, 0)) Nothing 12_346 1_000)
                      Nothing
                  )
            )
              #@?= ( pcon
                      ( PCommitFoldScott
                          (pconstant mockScriptHash1)
                          pdnothing
                          1_000
                          (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                          (pdjust (pdata $ pconstant 12_346))
                          100
                          2
                          (pconstant (pubKeyHashAddress mockPubKeyHash1))
                      )
                   )
        , testCase "the next expected ineligible node (by time) contributes 0 to the commitment" $
            ( nextState
                # pconstant
                  ( TxOut
                      (scriptHashAddress mockScriptHash1)
                      ( adaValue nodeAdaAmount
                          <> usdcValue 1_000
                          <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                      )
                      (outNode (Just (unwrapPubKeyHash mockPubKeyHash1, 0)) Nothing 12_347 1_000)
                      Nothing
                  )
            )
              #@?= ( pcon
                      ( PCommitFoldScott
                          (pconstant mockScriptHash1)
                          pdnothing
                          0
                          (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                          (pdjust (pdata $ pconstant 12_346))
                          100
                          2
                          (pconstant (pubKeyHashAddress mockPubKeyHash1))
                      )
                   )
        , testCase "the next expected ineligible node (by key) contributes 0 to the commitment" $
            ( pnextCommitState
                # pconstant mockCs2
                # pcon
                  ( PCommitFoldScott
                      (pconstant mockScriptHash1)
                      (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                      0
                      (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash1)) 0))
                      (pdjust (pdata $ pconstant 12_346))
                      100
                      1
                      (pconstant (pubKeyHashAddress mockPubKeyHash1))
                  )
                # pconstant
                  ( TxOut
                      (scriptHashAddress mockScriptHash1)
                      ( adaValue 8
                          <> usdcValue 1_000
                          <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                      )
                      (outNode (Just (unwrapPubKeyHash mockPubKeyHash2, 0)) Nothing 12_346 1_000)
                      Nothing
                  )
            )
              #@?= ( pcon
                      ( PCommitFoldScott
                          (pconstant mockScriptHash1)
                          pdnothing
                          0
                          (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash1)) 0))
                          (pdjust (pdata $ pconstant 12_346))
                          100
                          2
                          (pconstant (pubKeyHashAddress mockPubKeyHash1))
                      )
                   )
        , testCase "the next expected partially eligible node on the cutoff contributes its commitment minus the overcommitment" $
            ( pnextCommitState
                # pconstant mockCs2
                # pcon
                  ( PCommitFoldScott
                      (pconstant mockScriptHash1)
                      (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                      0
                      (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                      (pdjust (pdata $ pconstant 12_346))
                      100
                      1
                      (pconstant (pubKeyHashAddress mockPubKeyHash1))
                  )
                # pconstant
                  ( TxOut
                      (scriptHashAddress mockScriptHash1)
                      ( adaValue nodeAdaAmount
                          <> usdcValue 1_000
                          <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                      )
                      (outNode (Just (unwrapPubKeyHash mockPubKeyHash2, 0)) Nothing 12_346 1_000)
                      Nothing
                  )
            )
              #@?= ( pcon
                      ( PCommitFoldScott
                          (pconstant mockScriptHash1)
                          pdnothing
                          900
                          (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash2)) 0))
                          (pdjust (pdata $ pconstant 12_346))
                          100
                          2
                          (pconstant (pubKeyHashAddress mockPubKeyHash1))
                      )
                   )
        , testCase "fails when the node has an incorrect address" $
            pfails
              ( pnextCommitState
                  # pconstant mockCs2
                  # initialFirstCome
                  # pconstant
                    ( TxOut
                        (scriptHashAddress mockScriptHash2)
                        ( adaValue nodeAdaAmount
                            <> usdcValue 1_000
                            <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                        )
                        (outNode (Just (unwrapPubKeyHash mockPubKeyHash1, 0)) Nothing 12_345 1_000)
                        Nothing
                    )
              )
        , testCase "fails when the node doesn't have the expected key" $
            pfails
              ( pnextCommitState
                  # pconstant mockCs2
                  # initialFirstCome
                  # pconstant
                    ( TxOut
                        (scriptHashAddress mockScriptHash1)
                        ( adaValue nodeAdaAmount
                            <> usdcValue 1_000
                            <> singleton mockCs2 (TokenName (unwrapScriptHash mockScriptHash1)) 1
                        )
                        (outNode (Just (unwrapPubKeyHash mockPubKeyHash2, 0)) Nothing 12_345 1_000)
                        Nothing
                    )
              )
        ]

pisCommitFoldValueCorrectTests :: TestTree
pisCommitFoldValueCorrectTests =
  testGroup
    "pisCommitFoldValueCorrect"
    [ testCase "returns true on some ada and 1 commit fold token" $
        pisCommitFoldValueCorrect
          # pconstant mockCs
          # pconstant mockTn
          # (passertPositive # (passertSorted # pconstant (adaValue 2 <> singleton mockCs mockTn 1)))
          #@?= ptrue
    , testCase "returns false on some ada and 2 commit fold tokens" $
        pisCommitFoldValueCorrect
          # pconstant mockCs
          # pconstant mockTn
          # (passertPositive # (passertSorted # pconstant (adaValue 2 <> singleton mockCs mockTn 2)))
          #@?= pfalse
    , testCase "returns false on some ada and 1 commit fold token and 1 other token" $
        pisCommitFoldValueCorrect
          # pconstant mockCs
          # pconstant mockTn
          # (passertPositive # (passertSorted # pconstant (adaValue 2 <> singleton mockCs mockTn 1 <> singleton mockCs "other" 1)))
          #@?= pfalse
    , testCase "returs false on some ada and missing commit fold token" $
        pisCommitFoldValueCorrect
          # pconstant mockCs
          # pconstant mockTn
          # (passertPositive # (passertSorted # pconstant (adaValue 2)))
          #@?= pfalse
    ]
