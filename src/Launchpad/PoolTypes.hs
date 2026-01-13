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

data ConstantProductPoolDatum = ConstantProductPoolDatum
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''ConstantProductPoolDatum [('ConstantProductPoolDatum, 0)]
PlutusTx.makeLift ''ConstantProductPoolDatum

data PConstantProductPoolDatum (s :: S)
  = PConstantProductPoolDatum (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PConstantProductPoolDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PConstantProductPoolDatum where
  type PLifted PConstantProductPoolDatum = ConstantProductPoolDatum

deriving via
  (DerivePConstantViaData ConstantProductPoolDatum PConstantProductPoolDatum)
  instance
    (PConstantDecl ConstantProductPoolDatum)

instance PTryFrom PData PConstantProductPoolDatum

data PoolConstantProductDatum = PoolConstantProductDatum
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
  , poolSpecifics :: ConstantProductPoolDatum
  }
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''PoolConstantProductDatum [('PoolConstantProductDatum, 0)]
PlutusTx.makeLift ''PoolConstantProductDatum

data PPoolConstantProductDatum (s :: S)
  = PPoolConstantProductDatum
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
               , "poolSpecifics" ':= PConstantProductPoolDatum
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PIsData, PDataFields)

instance DerivePlutusType PPoolConstantProductDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PPoolConstantProductDatum where
  type PLifted PPoolConstantProductDatum = PoolConstantProductDatum

deriving via
  (DerivePConstantViaData PoolConstantProductDatum PPoolConstantProductDatum)
  instance
    (PConstantDecl PoolConstantProductDatum)

instance PTryFrom PData PPoolConstantProductDatum
instance PTryFrom PData (PAsData PPoolConstantProductDatum)
