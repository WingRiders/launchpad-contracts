module Launchpad.Constants where

import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2 (TokenName)

minutes :: Num a => a -> a
minutes m = m * 60 * 1_000

-- | The maximum number of tokens on one utxo.
maxUInt64 :: Integer
maxUInt64 = 9_223_372_036_854_775_807

-- | Min user index for multiple commitments
minNodeIndex :: Integer
minNodeIndex = 0

-- | Max user index for multiple commitments, the whole launch can have more nodes
maxNodeIndex :: Integer
maxNodeIndex = 255

separatorNodeKeyLength :: Integer
separatorNodeKeyLength = 1

-- | Required inactivity period for nodes. Used to disallow quick node removal just after adding.
nodesInactivityPeriod :: Integer
nodesInactivityPeriod = minutes 5

-- | PubKeyHashes are 28 bytes
expectedPkhLength :: Integer
expectedPkhLength = 28

{- | Number of milliseconds that should pass after the end of the launchpad
     for it to be considered failed and to allow emergency withdrawals.
-}
emergencyWithdrawalPeriod :: Integer
emergencyWithdrawalPeriod = minutes (60 * 24 * 30) -- 30 days

wrLpValidityTokenName :: TokenName
wrLpValidityTokenName = tokenName "L"

settingsNftName :: TokenName
settingsNftName = "settings"

-- | Used for bps calculations
bpsScalingFactor :: Num a => a
bpsScalingFactor = 10_000
