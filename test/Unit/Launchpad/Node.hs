module Unit.Launchpad.Node (nodeMintingPolicyTests, nodeUtilsTests) where

import Integration.Mock (nodeAdaAmount)
import Launchpad.Mint.Node
import Launchpad.Node
import Launchpad.Types
import Launchpad.Util
import Plutarch.Api.V1.Value
import Plutarch.Extra.Function
import Plutarch.Prelude
import Plutarch.Test
import Plutarch.Util hiding (passertPositive)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2 (Datum (..), OutputDatum (..), PubKeyHash (..), ScriptHash, toBuiltinData)
import PlutusLedgerApi.V2.Contexts
import Test.Tasty
import Test.Tasty.HUnit
import Unit.Launchpad.UtilFunctions (unwrapPubKeyHash, unwrapScriptHash)

nodeMintingPolicyTests :: TestTree
nodeMintingPolicyTests =
  testGroup
    "Node Minting Policy Tests"
    [ pinitialHeadDatumIsRightTests
    , pinitialHeadValueIsRightTests
    ]

nodeUtilsTests :: TestTree
nodeUtilsTests =
  testGroup
    "Node Utils Tests"
    [ pdelegateToFoldTests
    , pisNodeValueCorrectTests
    , pcheckSeparatorNodeTests
    ]

pinitialHeadDatumIsRightTests :: TestTree
pinitialHeadDatumIsRightTests =
  testGroup
    "pinitialHeadDatumIsRight"
    [ testCase "true on correct datum" $
        pinitialHeadDatumIsRight
          123_456_789
          (pconstant (Node Nothing Nothing 123_456_789 0))
          #@?= ptrue
    , testCase "false on time mismatch" $
        pinitialHeadDatumIsRight
          123_456_789
          (pconstant (Node Nothing Nothing 100 0))
          #@?= pfalse
    , testCase "false on non-zero committed" $
        pinitialHeadDatumIsRight
          123_456_789
          (pconstant (Node Nothing Nothing 123_456_789 456))
          #@?= pfalse
    , testCase "false on non-empty key" $
        pinitialHeadDatumIsRight
          123_456_789
          (pconstant (Node (Just ("123123", 0)) Nothing 123_456_789 0))
          #@?= pfalse
    , testCase "false on non-empty next" $
        pinitialHeadDatumIsRight
          123_456_789
          (pconstant (Node Nothing (Just ("123123", 0)) 123_456_789 0))
          #@?= pfalse
    ]

mockCs :: CurrencySymbol
mockCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c0"

mockTn :: TokenName
mockTn = "mock"

mockScriptHash1 :: ScriptHash
mockScriptHash1 = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c1"

mockScriptHash2 :: ScriptHash
mockScriptHash2 = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c2"

pinitialHeadValueIsRightTests :: TestTree
pinitialHeadValueIsRightTests =
  testGroup
    "pinitialHeadValueIsRight"
    [ testCase "true on correct value" $
        pinitialHeadValueIsRight
          (pconstant mockCs)
          (pconstant mockTn)
          (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton mockCs mockTn 1))))
          (pconstant nodeAdaAmount)
          #@?= ptrue
    , testCase "false on missing node token" $
        pinitialHeadValueIsRight
          (pconstant mockCs)
          (pconstant mockTn)
          (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount))))
          (pconstant nodeAdaAmount)
          #@?= pfalse
    , testCase "false on missing oil ada" $
        pinitialHeadValueIsRight
          (pconstant mockCs)
          (pconstant mockTn)
          (passertPositive # (passertSorted # (pconstant (singleton mockCs mockTn 1))))
          (pconstant nodeAdaAmount)
          #@?= pfalse
    , testCase "false on less oil ada value" $
        pinitialHeadValueIsRight
          (pconstant mockCs)
          (pconstant mockTn)
          (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken (nodeAdaAmount - 1) <> singleton mockCs mockTn 1))))
          (pconstant nodeAdaAmount)
          #@?= pfalse
    , testCase "true on more oil ada value" $
        pinitialHeadValueIsRight
          (pconstant mockCs)
          (pconstant mockTn)
          (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken (nodeAdaAmount + 1) <> singleton mockCs mockTn 1))))
          (pconstant nodeAdaAmount)
          #@?= ptrue
    , testCase "false on more than 1 node tokens" $
        pinitialHeadValueIsRight
          (pconstant mockCs)
          (pconstant mockTn)
          (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton mockCs mockTn 2))))
          (pconstant nodeAdaAmount)
          #@?= pfalse
    ]

pdelegateToFoldTests :: TestTree
pdelegateToFoldTests =
  testGroup
    "pdelegateToFold tests"
    [ testCase "delegate to fold fails on empty list" $
        pfails
          ( pdelegateToFold
              # (pconstant mockScriptHash1)
              # (pconstant mockScriptHash2)
              # (pconst # pconstant mockScriptHash1)
              # pconstant []
              # 0
          )
    , testCase "delegate to fold returns true on a correct singleton list" $
        pdelegateToFold
          # (pconstant mockScriptHash1)
          # (pconstant mockScriptHash2)
          # (pconst # pconstant mockScriptHash1)
          # pconstant [TxInInfo (TxOutRef "" 0) (TxOut (scriptHashAddress mockScriptHash2) mempty (OutputDatum (Datum (toBuiltinData ()))) Nothing)]
          # 0
          #@?= ptrue
    , testCase "delegate to fold returns false on an incorrect singleton list" $
        pdelegateToFold
          # (pconstant mockScriptHash1)
          # (pconstant mockScriptHash2)
          # (pconst # pconstant mockScriptHash1)
          # pconstant [TxInInfo (TxOutRef "" 0) (TxOut (scriptHashAddress mockScriptHash1) mempty (OutputDatum (Datum (toBuiltinData ()))) Nothing)]
          # 0
          #@?= pfalse
    , testCase "delegate to fold fails when the correct fold input doesn't have an inline datum" $
        pfails
          ( pdelegateToFold
              # (pconstant mockScriptHash1)
              # (pconstant mockScriptHash2)
              # (pconst # pconstant mockScriptHash1)
              # pconstant [TxInInfo (TxOutRef "" 0) (TxOut (scriptHashAddress mockScriptHash2) mempty NoOutputDatum Nothing)]
              # 0
          )
    , testCase "delegate to fold returns false when the fold input returns an incorrect node script hash" $
        pdelegateToFold
          # (pconstant mockScriptHash1)
          # (pconstant mockScriptHash2)
          # (pconst # pconstant mockScriptHash2)
          # pconstant [TxInInfo (TxOutRef "" 0) (TxOut (scriptHashAddress mockScriptHash1) mempty (OutputDatum (Datum (toBuiltinData ()))) Nothing)]
          # 0
          #@?= pfalse
    ]

mockIrrelevantCs :: CurrencySymbol
mockIrrelevantCs = "ffffffffff4da5d4c7bc8dc553f935543f7f004c2fc1a577a8fbffff"

mockIrrelevantTn :: TokenName
mockIrrelevantTn = "irrelevant"

mockPresaleCs :: CurrencySymbol
mockPresaleCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb0000"

mockPresaleTn :: TokenName
mockPresaleTn = "presale"

pisNodeValueCorrectTests :: TestTree
pisNodeValueCorrectTests =
  testGroup
    "pisNodeValueCorrect tests"
    [ testCase "true on 0 committed and 6 oil value for ADA" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton mockCs mockTn 1))))
          # 0
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "true on positive committed and 6 oil value for ADA" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken (1_000 + nodeAdaAmount) <> singleton mockCs mockTn 1))))
          # 1_000
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "true on providing more ADA than expected for ADA" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken (1_000 + nodeAdaAmount + 100) <> singleton mockCs mockTn 1))))
          # 1_000
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "false on 0 committed and insufficient oil value for ADA" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken (nodeAdaAmount - 1)
                                <> singleton mockCs mockTn 1
                            )
                        )
                  )
            )
          # 0
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "false on positive committed and insufficient oil value for ADA" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken (1_000 + nodeAdaAmount - 1)
                                <> singleton mockCs mockTn 1
                            )
                        )
                  )
            )
          # 1_000
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "true on 0 committed and 6 oil value for USDC" $
        pisNodeValueCorrect
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton mockCs mockTn 1))))
          # 0
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "true on positive committed and 6 oil value for USDC" $
        pisNodeValueCorrect
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton usdcSymbol usdcToken 1_000 <> singleton mockCs mockTn 1))
                  )
            )
          # 1_000
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "true on providing more ADA than expected for USDC" $
        pisNodeValueCorrect
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # (pconstant (singleton adaSymbol adaToken (nodeAdaAmount + 100) <> singleton usdcSymbol usdcToken 1_000 <> singleton mockCs mockTn 1))
                  )
            )
          # 1_000
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "false on providing more USDC than expected" $
        pisNodeValueCorrect
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton usdcSymbol usdcToken (1_000 + 100) <> singleton mockCs mockTn 1))
                  )
            )
          # 1_000
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "false on 0 committed and insufficient oil value for USDC" $
        pisNodeValueCorrect
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken (nodeAdaAmount - 1)
                                <> singleton mockCs mockTn 1
                            )
                        )
                  )
            )
          # 0
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "false on positive committed and insufficient oil value for USDC" $
        pisNodeValueCorrect
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken (nodeAdaAmount - 1)
                                <> singleton usdcSymbol usdcToken 1_000
                                <> singleton mockCs mockTn 1
                            )
                        )
                  )
            )
          # 1_000
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "false on missing the node token" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount))))
          # 0
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "false on multiple node tokens" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # (passertPositive # (passertSorted # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton mockCs mockTn 2))))
          # 0
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "false on providing more irrelevant tokens" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PDefault
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken nodeAdaAmount
                                <> singleton mockPresaleCs mockPresaleTn 1
                                <> singleton mockCs mockTn 1
                                <> singleton mockIrrelevantCs mockIrrelevantTn 1
                            )
                        )
                  )
            )
          # 0
          # pconstant nodeAdaAmount
          #@?= pfalse
    , testCase "true on providing one presale tier token for the presale tier" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PPresale
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken nodeAdaAmount
                                <> singleton mockPresaleCs mockPresaleTn 1
                                <> singleton mockCs mockTn 1
                            )
                        )
                  )
            )
          # 0
          # pconstant nodeAdaAmount
          #@?= ptrue
    , testCase "fails on providing no tokens for the presale tier" $
        pfails
          ( pisNodeValueCorrect
              # padaSymbol
              # padaToken
              # pconstant mockPresaleCs
              # pconstant mockCs
              # pconstant mockTn
              # pcon PPresale
              # ( passertPositive
                    # ( passertSorted
                          # (pconstant (singleton adaSymbol adaToken nodeAdaAmount <> singleton mockCs mockTn 1))
                      )
                )
              # 0
              # pconstant nodeAdaAmount
          )
    , testCase "false on providing two presale tokens for the presale tier" $
        pisNodeValueCorrect
          # padaSymbol
          # padaToken
          # pconstant mockPresaleCs
          # pconstant mockCs
          # pconstant mockTn
          # pcon PPresale
          # ( passertPositive
                # ( passertSorted
                      # ( pconstant
                            ( singleton adaSymbol adaToken nodeAdaAmount
                                <> singleton mockPresaleCs mockPresaleTn 2
                                <> singleton mockCs mockTn 1
                            )
                        )
                  )
            )
          # 0
          # pconstant nodeAdaAmount
          #@?= pfalse
    ]
  where
    usdcSymbol = "0123"
    usdcToken = "USDC"

-- | The length must be equal to 1 byte
mockSeparatorKey :: PubKeyHash
mockSeparatorKey = PubKeyHash "3"

-- | The length must be equal to 1 byte
mockSeparatorKey2 :: PubKeyHash
mockSeparatorKey2 = PubKeyHash "4"

-- | This one has a different length
mockShortSeparatorKey :: PubKeyHash
mockShortSeparatorKey = "3456789012345678901234567890123456789012345678901234567890123456789012"

mockPubKeyHash :: PubKeyHash
mockPubKeyHash = "4567890123"

pcheckSeparatorNodeTests :: TestTree
pcheckSeparatorNodeTests =
  testGroup
    "pcheckSeparatorNode tests"
    [ testCase "Returns the next field and increments the number of processed nodes on a correct last separator node" $
        pcheckSeparatorNode
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockCs
          # pconstant nodeAdaAmount
          # pconstant mockScriptHash1
          # 123_456_789
          # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
          # pconstant
            ( TxOut
                (scriptHashAddress mockScriptHash1)
                ( singleton adaSymbol adaToken nodeAdaAmount
                    <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                )
                ( OutputDatum
                    ( Datum
                        ( toBuiltinData
                            ( Node
                                (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                Nothing
                                123_456_789
                                0
                            )
                        )
                    )
                )
                Nothing
            )
          #@?= ppair pdnothing (pconstant @PInteger 1)
    , testCase "Returns the next field and increments the number of processed nodes on a correct non-last separator node" $
        pcheckSeparatorNode
          # pconstant usdcSymbol
          # pconstant usdcToken
          # pconstant mockCs
          # pconstant nodeAdaAmount
          # pconstant mockScriptHash1
          # 123_456_789
          # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
          # pconstant
            ( TxOut
                (scriptHashAddress mockScriptHash1)
                ( singleton adaSymbol adaToken nodeAdaAmount
                    <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                )
                ( OutputDatum
                    ( Datum
                        ( toBuiltinData
                            ( Node
                                (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                (Just (unwrapPubKeyHash mockPubKeyHash, 0))
                                123_456_789
                                0
                            )
                        )
                    )
                )
                Nothing
            )
          #@?= ppair
            (pdjust (pdata $ pbuiltinPair (pto (pconstant mockPubKeyHash)) (pconstant @PInteger 0)))
            (pconstant @PInteger 1)
    , testCase "Fails when the commit field is not 0" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    Nothing
                                    123_456_789
                                    1
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the key field is not equal to the expected one" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey2, 0))
                                    Nothing
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the createdTime is not equal to the current time" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    Nothing
                                    123_456_788
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the next field of the separator node is greater than the key field of the node" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey2)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey2, 0))
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the separator node has less oil ada" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken (nodeAdaAmount - 1)
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    Nothing
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the separator node has no node tokens" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    (singleton adaSymbol adaToken nodeAdaAmount)
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    Nothing
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the separator nodes has two node tokens" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 2
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    Nothing
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the separator node is placed on a different address" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash2)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockSeparatorKey, 0))
                                    Nothing
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    , testCase "Fails when the key is not 70 bytes long" $
        pfails
          ( pcheckSeparatorNode
              # pconstant usdcSymbol
              # pconstant usdcToken
              # pconstant mockCs
              # pconstant nodeAdaAmount
              # pconstant mockScriptHash1
              # 123_456_789
              # ppair (pdjust (pdata $ pbuiltinPair (pto (pconstant mockShortSeparatorKey)) 0)) 0
              # pconstant
                ( TxOut
                    (scriptHashAddress mockScriptHash1)
                    ( singleton adaSymbol adaToken nodeAdaAmount
                        <> singleton mockCs (TokenName (unwrapScriptHash mockScriptHash1)) 1
                    )
                    ( OutputDatum
                        ( Datum
                            ( toBuiltinData
                                ( Node
                                    (Just (unwrapPubKeyHash mockShortSeparatorKey, 0))
                                    Nothing
                                    123_456_789
                                    0
                                )
                            )
                        )
                    )
                    Nothing
                )
          )
    ]
  where
    usdcSymbol = "0123"
    usdcToken = "USDC"
