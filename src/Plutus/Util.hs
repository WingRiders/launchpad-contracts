module Plutus.Util where

import PlutusLedgerApi.V1.Value (
  AssetClass,
  adaSymbol,
  adaToken,
  assetClass,
 )
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  ScriptHash,
 )

scriptHashToAddress :: ScriptHash -> Address
scriptHashToAddress vh = Address (ScriptCredential vh) Nothing

adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken
