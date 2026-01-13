{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Util where

import Data.Map qualified as Map
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClass,
 )
import PlutusLedgerApi.V2

dummyCurrency :: CurrencySymbol
dummyCurrency = "648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198" -- free policy currency symbol

dummyTokens :: [AssetClass]
dummyTokens =
  [ assetClass dummyCurrency tn
  | tn <-
      [ "vBTC"
      , "vETH"
      , "vSOL"
      , "vADA"
      , "vUSDT"
      , "vDOGE"
      , "vMaxLengthTokenMaxLengthToken001"
      , "vMaxLengthTokenMaxLengthToken002"
      ]
  ]

coins :: Map.Map TokenName AssetClass
coins = Map.fromList [(snd $ unAssetClass ac, ac) | ac <- dummyTokens]

vBTC :: AssetClass
vBTC = coins Map.! "vBTC"

vETH :: AssetClass
vETH = coins Map.! "vETH"

vSOL :: AssetClass
vSOL = coins Map.! "vSOL"

vADA :: AssetClass
vADA = coins Map.! "vADA"

vUSDT :: AssetClass
vUSDT = coins Map.! "vUSDT"

vDOGE :: AssetClass
vDOGE = coins Map.! "vDOGE"
