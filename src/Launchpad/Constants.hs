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

lpValidityTokenName :: TokenName
lpValidityTokenName = tokenName "L"

{- |
  This is a fee that needs to be included in every request. It compensates the agents for the tx fee.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
agentFeeAda :: Num a => a
agentFeeAda = 2_000_000

{- |
  Swap fee is the fee that is returned to the pool for the liquidity providers.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
swapFeeInBasis :: Num a => a
swapFeeInBasis = 30

{- |
  Protocol fee is the fee that is placed in the treasury.
  It is located in the pool eUTxO, but is not counted towards the pool liquidity.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
protocolFeeInBasis :: Num a => a
protocolFeeInBasis = 5

{- |
  Project fee is the fee that is placed in the project treasury.
  It is located in the pool eUTxO, but is not counted towards the pool liquidity.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
projectFeeInBasis :: Num a => a
projectFeeInBasis = 0

{- |
  Reserve fee is the fee that is placed in the reserve treasury.
  It is located in the pool eUTxO, but is not counted towards the pool liquidity.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
reserveFeeInBasis :: Num a => a
reserveFeeInBasis = 0

-- | The basis of any fee
feeBasis :: Num a => a
feeBasis = 10_000

{- |
  This is the exact amount of share tokens minted for every pool.

  Note: This corresponds to the maximum amount of any token in a single UTxO
  (https://github.com/input-output-hk/cardano-ledger-specs/blob/master/alonzo/test/cddl-files/alonzo.cddl#L364)
-}
maxShareTokens :: Num a => a
maxShareTokens = maxInt64

-- | The maximum number of tokens on one utxo.
maxInt64 :: Num a => a
maxInt64 = 9_223_372_036_854_775_807

{- |
  This is sufficient min-ada that needs to be always present in pools and is paid by first liquidity provider.
-}
poolOilAda :: Num a => a
poolOilAda = 3_000_000

settingsNftName :: TokenName
settingsNftName = "settings"
