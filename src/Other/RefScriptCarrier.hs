{-# LANGUAGE BlockArguments #-}

module Other.RefScriptCarrier where

import Plutarch
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Types.Base (PTimestamps (..))
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

data RefScriptCarrierDatum = RefScriptCarrierDatum
  { owner :: PubKeyHash
  , deadline :: POSIXTime
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''RefScriptCarrierDatum [('RefScriptCarrierDatum, 0)]
PlutusTx.makeLift ''RefScriptCarrierDatum

data PRefScriptCarrierDatum (s :: S)
  = PRefScriptCarrierDatum
      ( Term
          s
          ( PDataRecord
              [ "owner" ':= PPubKeyHash
              , "deadline" ':= PPOSIXTime
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PIsData, PDataFields)

instance DerivePlutusType PRefScriptCarrierDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRefScriptCarrierDatum where
  type PLifted PRefScriptCarrierDatum = RefScriptCarrierDatum

deriving via
  (DerivePConstantViaData RefScriptCarrierDatum PRefScriptCarrierDatum)
  instance
    (PConstantDecl RefScriptCarrierDatum)

instance PTryFrom PData PRefScriptCarrierDatum

{- | This validates spending of a Reference Script Carrier eUTxO.
     In order to transaction be valid:
      1. The transaction has to be signed by the owner specified in datum
      2. The lower bound of the current time approximation must be equal or higher than the specified deadline
-}
prefScriptCarrierValidator :: Term s (PRefScriptCarrierDatum :--> PScriptContext :--> PBool)
prefScriptCarrierValidator = phoistAcyclic $ plam \datum context -> unTermCont do
  contextFields <- pletFieldsC @'["txInfo"] context
  txInfoFields <- pletFieldsC @'["signatories", "validRange"] contextFields.txInfo

  datumF <- pletFieldsC @'["owner", "deadline"] datum

  let signedByOwner = ptxSignedByPkh # datumF.owner # txInfoFields.signatories
  let timestamps = pfiniteTxValidityRangeTimestamps # txInfoFields.validRange
  PTimestamps lowerTime _ <- pmatchC timestamps

  pure $
    pand'List
      [ signedByOwner
      , lowerTime #>= datumF.deadline
      ]

refScriptCarrierValidator :: Term s PValidator
refScriptCarrierValidator = phoistAcyclic $ plam \rawDatum _redeemer context ->
  let datum = ptryFrom @PRefScriptCarrierDatum rawDatum fst
   in popaque $ perrorIfFalse #$ prefScriptCarrierValidator # datum # context

refScriptCarrierScriptValidator :: Script
refScriptCarrierScriptValidator = toScript refScriptCarrierValidator

refScriptCarrierScriptValidatorHash :: ScriptHash
refScriptCarrierScriptValidatorHash = scriptHash refScriptCarrierScriptValidator

refScriptCarrierScriptAddress :: Address
refScriptCarrierScriptAddress = scriptHashToAddress refScriptCarrierScriptValidatorHash

refScriptCarrierScript :: Script
refScriptCarrierScript = toScript refScriptCarrierValidator
