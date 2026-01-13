{-# LANGUAGE BlockArguments #-}

module Launchpad.PoolTypes where

import Plutarch
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Util ()
import PlutusLedgerApi.V2
import PlutusTx qualified

data MultisigScript
  = MultisigSignature BuiltinByteString
  | MultisigAllOf [MultisigScript]
  | MultisigAnyOf [MultisigScript]
  | MultisigAtLeast Integer [MultisigScript]
  | MultisigBefore Integer
  | MultisigAfter Integer
  | MultisigScript BuiltinByteString
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''MultisigScript
  [ ('MultisigSignature, 0)
  , ('MultisigAllOf, 1)
  , ('MultisigAnyOf, 2)
  , ('MultisigAtLeast, 3)
  , ('MultisigBefore, 4)
  , ('MultisigAfter, 5)
  , ('MultisigScript, 6)
  ]
PlutusTx.makeLift ''MultisigScript

data PMultisigScript (s :: S)
  = PMultisigSignature (Term s (PDataRecord '["keyHash" ':= PByteString]))
  | PMultisigAllOf (Term s (PDataRecord '["scripts" ':= PBuiltinList PMultisigScript]))
  | PMultisigAnyOf (Term s (PDataRecord '["scripts" ':= PBuiltinList PMultisigScript]))
  | PMultisigAtLeast (Term s (PDataRecord '["required" ':= PInteger, "scripts" ':= PBuiltinList PMultisigScript]))
  | PMultisigBefore (Term s (PDataRecord '["time" ':= PInteger]))
  | PMultisigAfter (Term s (PDataRecord '["time" ':= PInteger]))
  | PMultisigScript (Term s (PDataRecord '["scriptHash" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PMultisigScript where
  type DPTStrat _ = PlutusTypeData

-- Does it work even?
instance PTryFrom PData (PAsData (PBuiltinList PMultisigScript))
instance PTryFrom PData PMultisigScript

-- NOTE: we only care about the fee, everything else is ignored
data SundaeSettingsDatum = SundaeSettingsDatum
  { _settingsAdmin :: BuiltinData
  , _metadataAdmin :: BuiltinData
  , _treasuryAdmin :: BuiltinData
  , _treasuryAddress :: BuiltinData
  , _treasuryAllowance :: BuiltinData
  , _authorizedScoopers :: BuiltinData
  , _authorizedStakingKeys :: BuiltinData
  , _baseFee :: BuiltinData
  , _simpleFee :: BuiltinData
  , _strategyFee :: BuiltinData
  , poolCreationFee :: Integer
  , _extensions :: BuiltinData
  }
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed 'SundaeSettingsDatum [('SundaeSettingsDatum, 0)]
PlutusTx.makeLift ''SundaeSettingsDatum

data PSundaeSettingsDatum (s :: S)
  = PSundaeSettingsDatum
      ( Term
          s
          ( PDataRecord
              '[ "_settingsAdmin" ':= PData
               , "_metadataAdmin" ':= PData
               , "_treasuryAdmin" ':= PData
               , "_treasuryAddress" ':= PData
               , "_treasuryAllowance" ':= PData
               , "_authorizedScoopers" ':= PData
               , "_authorizedStakingKeys" ':= PData
               , "_baseFee" ':= PData
               , "_simpleFee" ':= PData
               , "_strategyFee" ':= PData
               , "poolCreationFee" ':= PInteger
               , "_extensions" ':= PData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSundaeSettingsDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSundaeSettingsDatum

-- Aiken's tuple is a builtin list of a limited length with
-- It can enforce the different types of elements, PlutusTx can't
type Asset = [BuiltinByteString]

data SundaePoolDatum = SundaePoolDatum
  { identifier :: BuiltinByteString
  , assets :: [Asset] -- Aiken enforces the length
  , circulatingLp :: Integer
  , bidFeesPer10Thousand :: Integer
  , askFeesPer10Thousand :: Integer
  , feeManager :: Maybe MultisigScript
  , marketOpen :: Integer
  , protocolFees :: Integer
  }
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''SundaePoolDatum [('SundaePoolDatum, 0)]
PlutusTx.makeLift ''SundaePoolDatum

type PAsset = PDataRecord '["currencySymbol" ':= PCurrencySymbol, "tokenName" ':= PTokenName]

data PSundaePoolDatum (s :: S)
  = PSundaePoolDatum
      ( Term
          s
          ( PDataRecord
              '[ "identifier" ':= PByteString
               , "assets" ':= PDataRecord '["a" ':= PAsset, "b" ':= PAsset]
               , "circulatingLp" ':= PInteger
               , "bidFeesPer10Thousand" ':= PInteger
               , "askFeesPer10Thousand" ':= PInteger
               , "feeManager" ':= PMaybeData PMultisigScript
               , "marketOpen" ':= PInteger
               , "protocolFees" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSundaePoolDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSundaePoolDatum

data WrConstantProductPoolDatum = WrConstantProductPoolDatum
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''WrConstantProductPoolDatum [('WrConstantProductPoolDatum, 0)]
PlutusTx.makeLift ''WrConstantProductPoolDatum

data PWrConstantProductPoolDatum (s :: S)
  = PWrConstantProductPoolDatum (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PWrConstantProductPoolDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PWrConstantProductPoolDatum where
  type PLifted PWrConstantProductPoolDatum = WrConstantProductPoolDatum

deriving via
  (DerivePConstantViaData WrConstantProductPoolDatum PWrConstantProductPoolDatum)
  instance
    (PConstantDecl WrConstantProductPoolDatum)

instance PTryFrom PData PWrConstantProductPoolDatum

data WrPoolConstantProductDatum = WrPoolConstantProductDatum
  { requestValidatorHash :: ScriptHash
  , assetASymbol :: CurrencySymbol
  , assetAToken :: TokenName
  , assetBSymbol :: CurrencySymbol
  , assetBToken :: TokenName
  , swapFeeInBasis :: Integer
  , protocolFeeInBasis :: Integer
  , projectFeeInBasis :: Integer
  , reserveFeeInBasis :: Integer
  , feeBasis :: Integer
  , agentFeeAda :: Integer
  , lastInteraction :: POSIXTime
  , treasuryA :: Integer
  , treasuryB :: Integer
  , projectTreasuryA :: Integer
  , projectTreasuryB :: Integer
  , reserveTreasuryA :: Integer
  , reserveTreasuryB :: Integer
  , projectBeneficiary :: Maybe Address
  , reserveBeneficiary :: Maybe Address
  , poolSpecifics :: WrConstantProductPoolDatum
  }
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''WrPoolConstantProductDatum [('WrPoolConstantProductDatum, 0)]
PlutusTx.makeLift ''WrPoolConstantProductDatum

data PWrPoolConstantProductDatum (s :: S)
  = PWrPoolConstantProductDatum
      ( Term
          s
          ( PDataRecord
              '[ "requestValidatorHash" ':= PScriptHash
               , "assetASymbol" ':= PCurrencySymbol
               , "assetAToken" ':= PTokenName
               , "assetBSymbol" ':= PCurrencySymbol
               , "assetBToken" ':= PTokenName
               , "swapFeeInBasis" ':= PInteger
               , "protocolFeeInBasis" ':= PInteger
               , "projectFeeInBasis" ':= PInteger
               , "reserveFeeInBasis" ':= PInteger
               , "feeBasis" ':= PInteger
               , "agentFeeAda" ':= PInteger
               , "lastInteraction" ':= PPOSIXTime
               , "treasuryA" ':= PInteger
               , "treasuryB" ':= PInteger
               , "projectTreasuryA" ':= PInteger
               , "projectTreasuryB" ':= PInteger
               , "reserveTreasuryA" ':= PInteger
               , "reserveTreasuryB" ':= PInteger
               , "projectBeneficiary" ':= PMaybeData (PAsData PAddress)
               , "reserveBeneficiary" ':= PMaybeData (PAsData PAddress)
               , "poolSpecifics" ':= PWrConstantProductPoolDatum
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PIsData, PDataFields)

instance DerivePlutusType PWrPoolConstantProductDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PWrPoolConstantProductDatum where
  type PLifted PWrPoolConstantProductDatum = WrPoolConstantProductDatum

deriving via
  (DerivePConstantViaData WrPoolConstantProductDatum PWrPoolConstantProductDatum)
  instance
    (PConstantDecl WrPoolConstantProductDatum)

instance PTryFrom PData PWrPoolConstantProductDatum
