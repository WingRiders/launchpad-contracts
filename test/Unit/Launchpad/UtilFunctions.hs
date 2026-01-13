{-# LANGUAGE BlockArguments #-}

module Unit.Launchpad.UtilFunctions where

import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Api.V2
import Plutarch.Extra.TermCont (pletFieldsC, pmatchC)
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude
import Plutarch.Util hiding (passertPositive)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2
import Test.Tasty

data SimpleState = SimpleState
  { adaCounter :: Integer
  , length :: Integer
  }

data PSimpleState (s :: S) = PSimpleState
  { adaCounter :: Term s PInteger
  , length :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PSimpleState where
  type DPTStrat _ = PlutusTypeScott

unwrapPubKeyHash :: PubKeyHash -> BuiltinByteString
unwrapPubKeyHash (PubKeyHash a) = a

unwrapScriptHash :: ScriptHash -> BuiltinByteString
unwrapScriptHash (ScriptHash a) = a

mockScriptHash :: ScriptHash
mockScriptHash = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c1"

mockValue :: Value
mockValue = singleton adaSymbol adaToken 1_000

mockTxIns :: Term s (PBuiltinList PTxInInfo)
mockTxIns =
  pconstant
    [ TxInInfo (TxOutRef "" 0) (TxOut (scriptHashAddress mockScriptHash) mockValue NoOutputDatum Nothing)
    , TxInInfo (TxOutRef "" 1) (TxOut (scriptHashAddress mockScriptHash) mockValue NoOutputDatum Nothing)
    , TxInInfo (TxOutRef "" 2) (TxOut (scriptHashAddress mockScriptHash) mockValue NoOutputDatum Nothing)
    ]

mockTxOuts :: Term s (PBuiltinList PTxOut)
mockTxOuts =
  pconstant
    [ TxOut (scriptHashAddress mockScriptHash) mockValue NoOutputDatum Nothing
    , TxOut (scriptHashAddress mockScriptHash) mockValue NoOutputDatum Nothing
    , TxOut (scriptHashAddress mockScriptHash) mockValue NoOutputDatum Nothing
    ]

mockInLocations1 :: Term s (PBuiltinList (PAsData PInteger))
mockInLocations1 = pcons # pdata 0 #$ pcons # pdata 1 #$ pcons # pdata 2 # pnil

mockOutLocations1 :: Term s (PBuiltinList (PAsData PInteger))
mockOutLocations1 = pcons # pdata 0 #$ pcons # pdata 1 #$ pcons # pdata 2 # pnil

mockOutLocations2 :: Term s (PBuiltinList (PAsData PInteger))
mockOutLocations2 = pcons # pdata 0 #$ pcons # pdata 1 #$ pcons # pdata (-1) # pnil

mockInitialState :: Term s PSimpleState
mockInitialState = pcon (PSimpleState 0 0)

-- | Function counting difference between input & output ada and number of ins & outs
countAdaDifference :: Term s (PSimpleState :--> PTxOut :--> PMaybe PTxOut :--> PSimpleState)
countAdaDifference = phoistAcyclic $ plam \state input output -> unTermCont do
  parsedS <- pmatchC state
  parsedI <- pletFieldsC @'["value"] input
  parsedO <- pletFieldsC @'["value"] (pfromJust # output)
  let newCount = parsedS.adaCounter + pvalueOf # parsedI.value # padaSymbol # padaToken - pif (pisNothing # output) 0 (pvalueOf # parsedO.value # padaSymbol # padaToken)
      newlength = parsedS.length + pif (pisNothing # output) 1 2

  pure (pcon (PSimpleState newCount newlength))

utilityFunctionsTests :: TestTree
utilityFunctionsTests = testGroup "Utility Functions Tests" []
