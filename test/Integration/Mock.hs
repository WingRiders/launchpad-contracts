module Integration.Mock where

import Data.Maybe (fromJust)
import Launchpad.Constants (maxUInt64)
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Other.FreePolicy (freeCurrencySymbol)
import Other.Vesting (vestingScriptValidatorHash)
import Plutus.Model.V2 (
  Run,
  adaValue,
  getMainUser,
  newUser,
  sendValue,
 )
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClass,
  assetClassValue,
 )
import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol (..),
  POSIXTime,
  PubKeyHash (..),
  ScriptHash (..),
  StakingCredential (..),
  TokenName (..),
  TxOutRef (..),
  Value,
 )
import Test.Util (
  vADA,
  vBTC,
  vDOGE,
  vETH,
  vSOL,
  vUSDT,
 )

data LaunchpadConfig = LaunchpadConfig
  { owner :: Address
  -- ^ The owner of the launch, supplies the project tokens
  , splitBps :: Integer
  -- ^ A value between 0 and 10_000, specifies a split of liquidity between Sundae and Wr pools
  -- 0 means everything goes to Sundae
  -- 10_000 means everything goes to Wr
  , wrPoolValidatorHash :: ScriptHash
  -- ^ The script hash of the Wr V2 Constant Product pool
  , wrFactoryValidatorHash :: ScriptHash
  -- ^ The script hash of the Wr V2 factory
  , wrPoolCurrencySymbol :: CurrencySymbol
  -- ^ The currency symbol of the Wr V2 pool policy
  , sundaePoolScriptHash :: ScriptHash
  -- ^ The script hash of the Sundae V3 pool
  , sundaeFeeTolerance :: Integer
  -- ^ The max amount of ada it is tolerable to pay to create a Sundae pool
  -- Note that the actual amount is hopefully less and is controlled by the Sundae settings utxo.
  -- In case the settings specify a value above the tolerance, no Sundae pool is created.
  , sundaeSettingsCurrencySymbol :: CurrencySymbol
  -- ^ The currency symbol of the Sundae settings NFT.
  , startTime :: POSIXTime
  -- ^ The start time must be set to the lowest of the tiers start times.
  , endTime :: POSIXTime
  -- ^ The time after which users no longer can contribute to nor withdraw from the launch.
  , projectToken :: AssetClass
  -- ^ The asset that is being launched
  , raisingToken :: AssetClass
  -- ^ The assets that is being raised
  , projectMinCommitment :: Integer
  -- ^ The min possible amount of tokens the launchpad can raise.
  -- In case less raised tokens are collected, the launch is considered failed and tokens are returned back.
  , projectMaxCommitment :: Integer
  -- ^ The maximum amount of tokens the launchpad can raise.
  -- Can be set to max int64 value to essentially remove the cap.
  , totalTokens :: Integer
  -- ^ The total number of the project tokens committed to the launchpad.
  , tokensToDistribute :: Integer
  -- ^ The number of the project tokens to distribute among the launchpad users.
  , raisedTokensPoolPartPercentage :: Integer
  -- ^ The percentage of the raised tokens to place into the pool.
  , daoFeeNumerator :: Integer
  -- ^ Controls the dao fee collected in raised tokens
  , daoFeeDenominator :: Integer
  -- ^ Controls the dao fee collected in raised tokens
  , daoFeeReceiver :: Address
  -- ^ Controls the address where the dao fee is sent
  , daoAdmin :: PubKeyHash
  -- ^ Controls the pub key hash of a dao admin.
  -- This signer can add separator nodes (the launch owner can do that as well).
  , collateral :: Integer
  -- ^ How much collateral (in Lovelace) is locked into the launch
  -- Must be at least 2 ada per used DEX plus 2 ada for the dao fee utxo
  -- The rest is returned if the launch is succesful.
  -- If the launch is failed (not cancelled), the collateral is split between the commit fold owner and the dao fee receiver
  , starter :: TxOutRef
  -- ^ The tx out ref of the utxo that has to be spent to uniquely identify a launch
  , vestingPeriodDuration :: POSIXTime
  -- ^ Configures the duration period of the vesting utxo which holds the owner's shares
  , vestingPeriodDurationToFirstUnlock :: POSIXTime
  -- ^ Configures the duration period to first unlock of the vesting utxo which holds the owner's shares
  , vestingPeriodInstallments :: Integer
  -- ^ Configures the number of installments of the vesting utxo which holds the owner's shares
  , vestingPeriodStart :: POSIXTime
  -- ^ Configures the start of the vesting utxo which holds the owner's shares
  , vestingValidatorHash :: ScriptHash
  -- ^ Configures the validator hash of the vesting utxo which holds the owner's shares
  , presaleTierCs :: CurrencySymbol
  -- ^ The currency symbol of the presale tier token
  -- Note that the token name is not checked.
  -- That allows using NFTs on the same policy as presale tokens.
  , presaleTierStartTime :: POSIXTime
  -- ^ The commitment start time of the presale tier
  , defaultStartTime :: POSIXTime
  -- ^ The commitment start time of the default tier
  , presaleTierMinCommitment :: Integer
  -- ^ The min user commitment of the presale tier
  , defaultTierMinCommitment :: Integer
  -- ^ The min user commitment of the default tier
  , presaleTierMaxCommitment :: Integer
  -- ^ The max user commitment of the presale tier
  , defaultTierMaxCommitment :: Integer
  -- ^ The max user commitment of the  tier
  , nodeAda :: Integer
  -- ^ The amount of ada a commitment node must hold.
  -- This ada is used up to compensate the folds and to create a user rewards holder.
  , commitFoldFeeAda :: Integer
  -- ^ The amount of ada the commit fold gets per each folded node.
  , oilAda :: Integer
  -- ^ The min amount of ada various utxos are expected to held.
  -- This includes the rewards holder, dao fee, and final project tokens holder.
  , sundaeFee :: Integer
  -- ^ Not a part of contracts configuration, used for tests only
  }
  deriving (Show, Eq, Ord)

data Wallets = Wallets
  { userWallet1 :: PubKeyHash
  , userWallet2 :: PubKeyHash
  , userWallet3 :: PubKeyHash
  , poolInitWallet :: PubKeyHash
  , launchpadOwner :: PubKeyHash
  , daoFeeReceiver :: PubKeyHash
  , daoAdmin :: PubKeyHash
  }

defaultTokens :: Value
defaultTokens =
  adaValue 100_000_000
    <> assetClassValue vBTC 1_000_000
    <> assetClassValue vETH 1_000_000
    <> assetClassValue vSOL 1_000_000
    <> assetClassValue vADA 1_000_000
    <> assetClassValue vUSDT 1_000_000
    <> assetClassValue vDOGE 1_000_000

setupWallets :: LaunchpadConfig -> Run Wallets
setupWallets config = do
  w1 <- newUser defaultTokens
  w2 <- newUser defaultTokens
  w3 <- newUser defaultTokens
  poolInitWallet <-
    newUser $
      defaultTokens
        <> assetClassValue (assetClass mockWrPoolCurrencySymbol "lpShare") 1_000
        <> assetClassValue (assetClass mockWrPoolCurrencySymbol C.wrLpValidityTokenName) 1
  admin <- getMainUser

  -- This initializes wallets **#6** and **#7** and adds them to the Mock,
  -- They are later used as the mockOwnerPkh and the daoAdmin from defaultLaunchpadConfig
  -- This is done because plutus-simple-model generates a signing key based on the wallet index, from which is then PKH derived
  _ <- newUser mempty
  _ <- newUser mempty

  let launchpadOwner = fromJust (toPubKeyHash config.owner)
      daoFeeReceiver = fromJust (toPubKeyHash config.daoFeeReceiver)
      daoAdmin = config.daoAdmin

  _ <- sendValue admin (defaultTokens <> assetClassValue adaAssetClass config.collateral) launchpadOwner
  _ <- sendValue admin defaultTokens daoFeeReceiver
  _ <- sendValue admin defaultTokens daoAdmin
  return
    Wallets
      { userWallet1 = w1
      , userWallet2 = w2
      , userWallet3 = w3
      , poolInitWallet
      , launchpadOwner
      , daoFeeReceiver
      , daoAdmin
      }

mockStarterRef :: TxOutRef
mockStarterRef = TxOutRef "f2a51f02852b8ecdc0b5eac198ed33d198b93847566c2c4410fc7e2af6e1148d" 0

mockSundaePoolScriptHash :: ScriptHash
mockSundaePoolScriptHash = ScriptHash hash
  where
    CurrencySymbol hash = freeCurrencySymbol

mockWrPoolValidatorHash :: ScriptHash
mockWrPoolValidatorHash = "36bea2acff0a1c9376b0fd4137ee46fb0f7acfd173ec071e338f8000"

-- ScriptHash of the mockFactoryScript from Integration.Launchpad.Validators
mockWrFactoryValidatorHash :: ScriptHash
mockWrFactoryValidatorHash = "52c6af0c9b744b4eecce838538a52ceb155038b3de68e2bb2fa8fc37"

mockWrPoolCurrencySymbol :: CurrencySymbol
mockWrPoolCurrencySymbol = freeCurrencySymbol

mockVestingScriptHash :: ScriptHash
mockVestingScriptHash = vestingScriptValidatorHash

mockPresaleCs :: CurrencySymbol
mockPresaleCs = "ff614bd7804da5d4c7bc8dc553f935543f7f004c2fc1a577a8fb96c2"

-- This is a PKH generated by Plutus-Simple-Model on index 6.
mockOwnerPkh :: PubKeyHash
mockOwnerPkh = "acdf471662382385725e8ad8b2a4b85ddb61a4e07d7fc1b568213ca8"

mockDaoFeeReceiverPkh :: PubKeyHash
mockDaoFeeReceiverPkh = "0277988e2ef9010ba67ef2d6a49d7ccaa65b05cc1a7587599a7da044"

-- This is a PKH generated by Plutus-Simple-Model on index 7.
mockDaoAdminPkh :: PubKeyHash
mockDaoAdminPkh = "0277988e2ef9010ba67ef2d6a49d7ccaa65b05cc1a7587599a7da044"

mockGenericPkh :: PubKeyHash
mockGenericPkh = "12345678901234567890123456789012345678901234567890123455"

mockGenericPkh2 :: PubKeyHash
mockGenericPkh2 = "12345678901234567890123456789012345678901234567890123456"

mockStakingCredential :: StakingCredential
mockStakingCredential = StakingHash (PubKeyCredential mockGenericPkh)

mockRequestScriptHash :: ScriptHash
mockRequestScriptHash = "00000000000000000000000000000000000000000000000000000000"

-- | The number of ADA that must be locked into each user node.
nodeAdaAmount :: Integer
nodeAdaAmount = oilAdaAmount + commitFoldFeeAdaAmount + rewardsFoldFeeAdaAmount

-- | The number of oil ADA that must be locked into various utxo.
oilAdaAmount :: Integer
oilAdaAmount = 2_000_000

-- | Fee given to the commit fold owner per a "commit-folded" node.
commitFoldFeeAdaAmount :: Integer
commitFoldFeeAdaAmount = 48_500

-- | Fee given to the rewards fold owner per a "rewards-folded" node.
rewardsFoldFeeAdaAmount :: Integer
rewardsFoldFeeAdaAmount = 177_000

mockSundaeFeeTolerance :: Integer
mockSundaeFeeTolerance = 100_000_000

mockSundaeSettingsCurrencySymbol :: CurrencySymbol
mockSundaeSettingsCurrencySymbol = "112233445566778899bc8dc553f935543f7f004c2fc1a577a8fb96c4"

defaultLaunchpadConfig :: LaunchpadConfig
defaultLaunchpadConfig =
  LaunchpadConfig
    { owner = Address (PubKeyCredential mockOwnerPkh) Nothing
    , splitBps = 10_000
    , wrPoolValidatorHash = mockWrPoolValidatorHash
    , wrFactoryValidatorHash = mockWrFactoryValidatorHash
    , wrPoolCurrencySymbol = mockWrPoolCurrencySymbol
    , sundaePoolScriptHash = mockSundaePoolScriptHash
    , sundaeFeeTolerance = mockSundaeFeeTolerance
    , sundaeSettingsCurrencySymbol = mockSundaeSettingsCurrencySymbol
    , startTime = 20_000 -- Starts in Slot 20
    , endTime = 750_000 -- Ends in Slot 750
    , projectToken = vBTC
    , raisingToken = adaAssetClass
    , projectMinCommitment = 1_000
    , projectMaxCommitment = 1_000_000_000_000 -- Max. 1M ADA
    , totalTokens = 1_000_000
    , tokensToDistribute = 700_000
    , raisedTokensPoolPartPercentage = 50
    , daoFeeNumerator = 5
    , daoFeeDenominator = 100
    , daoFeeReceiver = Address (PubKeyCredential mockDaoFeeReceiverPkh) Nothing
    , collateral = 100_000_000
    , daoAdmin = mockDaoAdminPkh
    , starter = mockStarterRef
    , vestingPeriodDuration = 0
    , vestingPeriodDurationToFirstUnlock = 0
    , vestingPeriodInstallments = 0
    , vestingPeriodStart = 500_000 + 50_000
    , vestingValidatorHash = mockVestingScriptHash
    , presaleTierCs = mockPresaleCs
    , presaleTierMinCommitment = 1
    , defaultTierMinCommitment = 1
    , presaleTierMaxCommitment = 10_000_000
    , defaultTierMaxCommitment = maxUInt64
    , presaleTierStartTime = 100_000
    , defaultStartTime = 200_000
    , nodeAda = nodeAdaAmount
    , commitFoldFeeAda = commitFoldFeeAdaAmount
    , oilAda = oilAdaAmount
    , sundaeFee = 0
    }

agentFeeAdaWr :: Num a => a
agentFeeAdaWr = 2_000_000

swapFeeInBasisWr :: Num a => a
swapFeeInBasisWr = 30

protocolFeeInBasisWr :: Num a => a
protocolFeeInBasisWr = 5

projectFeeInBasisWr :: Num a => a
projectFeeInBasisWr = 0

reserveFeeInBasisWr :: Num a => a
reserveFeeInBasisWr = 0

-- | The basis of any fee
feeBasisWr :: Num a => a
feeBasisWr = 10_000

wrDatum :: AssetClass -> AssetClass -> WrPoolConstantProductDatum
wrDatum projectToken raisingToken =
  WrPoolConstantProductDatum
    { requestValidatorHash = mockRequestScriptHash
    , assetASymbol
    , assetAToken
    , assetBSymbol
    , assetBToken
    , lastInteraction = 0
    , treasuryA = 0
    , treasuryB = 0
    , projectTreasuryA = 0
    , projectTreasuryB = 0
    , reserveTreasuryA = 0
    , reserveTreasuryB = 0
    , agentFeeAda = agentFeeAdaWr
    , swapFeeInBasis = swapFeeInBasisWr
    , protocolFeeInBasis = protocolFeeInBasisWr
    , projectFeeInBasis = projectFeeInBasisWr
    , reserveFeeInBasis = reserveFeeInBasisWr
    , feeBasis = feeBasisWr
    , projectBeneficiary = Nothing
    , reserveBeneficiary = Nothing
    , poolSpecifics = WrConstantProductPoolDatum
    }
  where
    (AssetClass (assetASymbol, assetAToken), AssetClass (assetBSymbol, assetBToken)) = if projectToken < raisingToken then (projectToken, raisingToken) else (raisingToken, projectToken)

sundaeDatum :: BuiltinByteString -> AssetClass -> AssetClass -> Integer -> Integer -> SundaePoolDatum
sundaeDatum identifier projectToken raisingToken circulatingLp protocolFees =
  SundaePoolDatum
    { identifier
    , assets
    , circulatingLp
    , bidFeesPer10Thousand = 35
    , askFeesPer10Thousand = 35
    , feeManager = Nothing
    , marketOpen = 1
    , protocolFees
    }
  where
    (AssetClass (assetASymbol, assetAToken), AssetClass (assetBSymbol, assetBToken)) =
      if projectToken < raisingToken
        then (projectToken, raisingToken)
        else (raisingToken, projectToken)
    assets = [[unCurrencySymbol assetASymbol, unTokenName assetAToken], [unCurrencySymbol assetBSymbol, unTokenName assetBToken]]
