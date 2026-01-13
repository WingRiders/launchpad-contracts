module Unit.Tests (unitTestsOther, unitTestsLaunchpad) where

import Data.Default (def)
import Plutarch
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Prelude
import Plutarch.Test (
  pfails,
  psucceeds,
  (#@?=),
 )
import Plutarch.Util
import PlutusLedgerApi.V2
import Test.Tasty (
  TestTree,
  testGroup,
 )
import Test.Tasty.HUnit (
  testCase,
  (@?=),
 )
import Unit.Launchpad.Tests

unitTestsOther :: TestTree
unitTestsOther =
  testGroup
    "Unit tests - Other"
    [ passertPositivePositive
    , passertPositiveNegative
    , passertPositiveZero
    , pasIntFails
    , ppermissiveParsing
    , p2elemsAtTests
    , pfromPDatumTests
    , ptryFromTests
    ]
  where
    passertPositivePositive =
      testCase "passertPositive succeeds on positive integers" $ psucceeds (Plutarch.Util.passertPositive # 1)
    passertPositiveNegative =
      testCase "passertPositive fails on negative integers" $ pfails (Plutarch.Util.passertPositive # (-1))
    passertPositiveZero = testCase "passertPositive fails on zero" $ pfails (Plutarch.Util.passertPositive # 0)
    pasIntFails = testCase "pasInt fails on non-integer" $ pfails (pasInt # pforgetData (pdata pmockTxOutAdaOnly))

unitTestsLaunchpad :: TestTree
unitTestsLaunchpad =
  testGroup
    "Unit tests - Launchpad"
    [launchpadUnitTests]

p2elemsAtTests :: TestTree
p2elemsAtTests =
  testGroup
    "p2elemsAt tests"
    [ testCase "returns a pair with the correct values" $
        (p2elemsAt @PBuiltinList @PInteger # 0 # 1 # pconstant [1, 2, 3, 4, 5])
          #@?= ppair (pconstant @PInteger 1) (pconstant @PInteger 2)
    , testCase "fails if the index is out of bounds" $
        pfails (p2elemsAt @PBuiltinList @PInteger # 0 # 8 # pconstant [1, 2, 3])
    , testCase "fails if the index is negative" $
        pfails (p2elemsAt @PBuiltinList @PInteger # 0 # (-1) # pconstant [1, 2, 3])
    , testCase "fails if passed two equal indices" $
        pfails (p2elemsAt @PBuiltinList @PInteger # 0 # 0 # pconstant [1, 2, 3])
    ]

pfromPDatumTests :: TestTree
pfromPDatumTests =
  testGroup
    "pfromPDatum tests"
    [ testCase "pfromPDatum succeeds on datum with the correct type" $
        psucceeds $
          pfromPDatum @PermissiveParsing1 # pcon (PDatum (pforgetData (pdata (pcon (PermissiveParsing1 (pdcons # pdata 0 # pdnil))))))
    , testCase "pfromPDatum fails on datum with invalid type" $
        pfails $
          pfromPDatum @PermissiveParsing1 # pcon (PDatum (pforgetData (pdata (pdcons # pdata (pconstant @PByteString "a") # pdnil))))
    , testCase "succeeds on casting a correct bytestring to a pubkeyhash" $
        psucceeds $
          pfromPDatum @(PAsData PPubKeyHash) # pcon (PDatum (pforgetData (pdata (pconstant @PByteString "aaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
    , testCase "fails on casting a short bytestring to a pubkeyhash" $
        pfails $
          pfromPDatum @(PAsData PPubKeyHash) # pcon (PDatum (pforgetData (pdata (pconstant @PByteString "a"))))
    ]

ptryFromTests :: TestTree
ptryFromTests =
  testGroup
    "ptryFrom tests"
    [ testCase "ptryFrom succeeds on datum with the correct type" $
        psucceeds $
          ptryFrom @PermissiveParsing1
            (pforgetData (pdata (pcon (PermissiveParsing1 (pdcons # pdata 0 # pdnil)))))
            fst
    , testCase "ptryFrom fails on datum with invalid type" $
        pfails $
          ptryFrom @PermissiveParsing1
            (pforgetData (pdata (pdcons # pdata (pconstant @PByteString "a") # pdnil)))
            fst
    ]

data PermissiveParsing1 (s :: S) = PermissiveParsing1 (Term s (PDataRecord '["_0" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance PTryFrom PData PermissiveParsing1

instance DerivePlutusType PermissiveParsing1 where
  type DPTStrat _ = PlutusTypeData

data PermissiveParsing2 (s :: S)
  = PermissiveParsing2
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PInteger
               , "_1" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PermissiveParsing2 where
  type DPTStrat _ = PlutusTypeData

parse1 :: Term s (PermissiveParsing1 :--> PInteger)
parse1 = plam $ \v -> pletFields @'["_0"] v $ \p -> p._0

parse2 :: Term s (PermissiveParsing2 :--> PInteger)
parse2 = plam $ \v -> pletFields @'["_0"] v $ \p -> p._0

ppermissiveParsing :: TestTree
ppermissiveParsing =
  testCase "The data records are parsed permissively" $ printTerm def parse1 @?= printTerm def parse2

someAda :: Value
someAda = singleton adaSymbol adaToken 1_000

mockPubKeyAddr :: Address
mockPubKeyAddr = Address (PubKeyCredential "0123") Nothing

mockTxOutAdaOnly :: TxOut
mockTxOutAdaOnly = TxOut mockPubKeyAddr someAda NoOutputDatum Nothing

pmockTxOutAdaOnly :: Term s PTxOut
pmockTxOutAdaOnly = pconstant mockTxOutAdaOnly
