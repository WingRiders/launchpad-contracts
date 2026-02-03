{-# LANGUAGE AllowAmbiguousTypes #-}

module Integration.Util where

import Cardano.Simple.Ledger.Tx qualified as Tx
import Data.List (find)
import Data.Maybe
import GHC.Stack (HasCallStack)
import Integration.Mock (LaunchpadConfig (..), mockSundaeSettingsCurrencySymbol, mockWrPoolCurrencySymbol)
import Launchpad.CommitFold qualified as C
import Launchpad.Constants qualified as C
import Launchpad.FailProof qualified as FP
import Launchpad.Mint.CommitFold qualified as C
import Launchpad.Mint.FailProof qualified as FP
import Launchpad.Mint.Node qualified as N
import Launchpad.Mint.PoolProof
import Launchpad.Mint.ProjectTokensHolder qualified as PTH
import Launchpad.Mint.RewardsFold qualified as R
import Launchpad.Node qualified as N
import Launchpad.PoolProof
import Launchpad.ProjectTokensHolderFinal qualified as PTHF
import Launchpad.ProjectTokensHolderFirst qualified as PTHFirst
import Launchpad.RewardsFold qualified as R
import Launchpad.RewardsHolder qualified as RH
import Launchpad.Types

import Plutarch.Extra.ScriptContext (scriptHashToTokenName)

import Plutus.Model

import Launchpad.Constants (settingsNftName)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol (..), TokenName (..), assetClass, assetClassValue)
import PlutusLedgerApi.V1.Value qualified as V
import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Datum (..),
  FromData (..),
  OutputDatum (..),
  POSIXTime (..),
  PubKeyHash (..),
  ToData (..),
  TxOut (..),
  Value (..),
  singleton,
 )
import PlutusTx.Builtins (blake2b_256)
import PlutusTx.Prelude (encodeUtf8)
import Test.Tasty
import Test.Util (
  vADA,
  vBTC,
  vDOGE,
  vETH,
  vSOL,
  vUSDT,
 )
import Unit.Launchpad.UtilFunctions (unwrapScriptHash)
import Prelude

ensureTx :: PubKeyHash -> Tx -> Run ()
ensureTx submitter tx =
  signTx submitter tx >>= sendTx >>= \case
    Right _ -> pure ()
    Left e -> fail $ show e

data AddToken = AddToken | SkipToken

isValidatorInput :: HasValidator a => a -> Tx.TxIn -> Bool
isValidatorInput v i = case i.txInType of
  Just (Tx.ConsumeScriptAddress (Just v') _ _) -> v' == toV2 (toValidator v)
  _ -> False

hasDatumType :: forall a. FromData a => Tx.TxIn -> Bool
hasDatumType i = case i.txInType of
  Just (Tx.ConsumeScriptAddress _ _ d) -> isJust (fromBuiltinData @a (toBuiltinData d))
  _ -> False

tryDatumTypeIndex :: forall a. FromData a => [(Tx.TxIn, Integer)] -> Integer
tryDatumTypeIndex inputs = snd . fromJust . find (hasDatumType @a . fst) $ inputs

txOutputDatum :: (HasCallStack, FromData a) => TxOut -> a
txOutputDatum txOut = case txOutDatum txOut of
  OutputDatum (Datum d) -> fromJust (fromBuiltinData (toBuiltinData d))
  _ -> error "txOutputDatum: no datum"

unwrapPubKeyHash :: PubKeyHash -> BuiltinByteString
unwrapPubKeyHash (PubKeyHash b) = b

memptyIf :: Monoid a => (b -> Bool) -> b -> (b -> a) -> a
memptyIf p b f = if p b then mempty else f b

memptyIfZero :: Value -> Value
memptyIfZero value = memptyIf V.isZero value id

wrShareTokenName :: AssetClass -> AssetClass -> TokenName
wrShareTokenName assetA assetB =
  TokenName . blake2b_256 $
    blake2b_256 "0"
      <> blake2b_256 (encodeUtf8 "1")
      <> blake2b_256 (encodeUtf8 "1")
      <> blake2b_256 (aSymbol <> aToken)
      <> blake2b_256 (bSymbol <> bToken)
  where
    AssetClass (CurrencySymbol aSymbol, TokenName aToken) = assetA
    AssetClass (CurrencySymbol bSymbol, TokenName bToken) = assetB

addressToPubKeyHash :: Address -> PubKeyHash
addressToPubKeyHash addr = fromJust $ toPubKeyHash addr

-- https://github.com/SundaeSwap-finance/sundae-contracts/blob/be33466b7dbe0f8e6c0e0f46ff23737897f45835/lib/shared.ak#L222
poolSundaeNftName :: BuiltinByteString -> TokenName
poolSundaeNftName identifier = TokenName $ "000de140" <> identifier

-- https://github.com/SundaeSwap-finance/sundae-contracts/blob/be33466b7dbe0f8e6c0e0f46ff23737897f45835/lib/shared.ak#L228
poolSundaeLpName :: BuiltinByteString -> TokenName
poolSundaeLpName identifier = TokenName $ "0014df10" <> identifier

mockSundaeIdentifier :: BuiltinByteString
mockSundaeIdentifier = "AAAFFF"

good :: LaunchpadConfig -> String -> (LaunchpadConfig -> Run a) -> TestTree
good config msg t =
  testNoErrors
    ( adaValue 100_000_000_000_000
        <> singleton mockSundaeSettingsCurrencySymbol settingsNftName 1
        <> assetClassValue vBTC 100_000_000_000_000
        <> assetClassValue vETH 100_000_000_000_000
        <> assetClassValue vSOL 100_000_000_000_000
        <> assetClassValue vADA 100_000_000_000_000
        <> assetClassValue vUSDT 100_000_000_000_000
        <> assetClassValue vDOGE 100_000_000_000_000
        <> singleton (CurrencySymbol (unwrapScriptHash config.sundaePoolScriptHash)) (poolSundaeLpName mockSundaeIdentifier) 1_000_000
        <> singleton (CurrencySymbol (unwrapScriptHash config.sundaePoolScriptHash)) (poolSundaeNftName mockSundaeIdentifier) 1
        <> assetClassValue (assetClass mockWrPoolCurrencySymbol "lpShare") 1_000
        <> assetClassValue (assetClass mockWrPoolCurrencySymbol C.wrLpValidityTokenName) 1
        -- ProjectTokensHolder Validity Token (in the Admin wallet [getMainUser])
        <> assetClassValue (assetClass (PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig config)) (scriptHashToTokenName (PTHFirst.projectTokensHolderScriptValidatorHash (firstTokensHolderConfig config)))) 1
        <> assetClassValue
          ( assetClass
              (N.nodeMintingPolicySymbol (nodePolicyConfig config))
              (scriptHashToTokenName (N.nodeScriptValidatorHash (nodeConfig config)))
          )
          100_000
        <> assetClassValue
          ( assetClass
              (C.commitFoldMintingPolicySymbol (commitFoldPolicyConfig config))
              (scriptHashToTokenName (C.commitFoldScriptValidatorHash (commitFoldConfig config)))
          )
          2
        <> assetClassValue
          ( assetClass
              (R.rewardsFoldMintingPolicySymbol (rewardsFoldPolicyConfig config))
              (scriptHashToTokenName (R.rewardsFoldScriptValidatorHash (rewardsFoldConfig config)))
          )
          1
        <> assetClassValue
          ( assetClass
              (PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig config))
              (scriptHashToTokenName (PTHFirst.projectTokensHolderScriptValidatorHash (firstTokensHolderConfig config)))
          )
          2
        <> assetClassValue
          ( assetClass
              FP.failProofPolicySymbol
              (scriptHashToTokenName FP.failProofScriptValidatorHash)
          )
          1
        <> assetClassValue config.projectToken config.totalTokens
        <> assetClassValue config.raisingToken 100_000_000_000_000
        <> V.scale 100_000 (tierToken config Presale)
    )
    defaultBabbageV2
    msg
    (t config)

tierToken :: LaunchpadConfig -> Tier -> Value
tierToken _ Default = mempty
tierToken config Presale = singleton config.presaleTierCs "" 1

bad :: LaunchpadConfig -> String -> (LaunchpadConfig -> Run a) -> TestTree
bad config msg t = good config msg (mustFail . t)

{- | currentTime provides ending 'POSIXTime' of a 'Slot'.
  This function gets the starting 'POSIXTime' of a 'Slot', that is actually used for the validity range.
-}
actualTime :: Run POSIXTime
actualTime = do
  slotUpperBound <- currentTime
  return $ slotUpperBound - 999

nodeConfig :: LaunchpadConfig -> N.NodeConfig
nodeConfig lCfg@LaunchpadConfig {..} =
  N.NodeConfig
    { starter
    , nodeSymbol = N.nodeMintingPolicySymbol (nodePolicyConfig lCfg)
    , rewardsFoldSymbol = R.rewardsFoldMintingPolicySymbol (rewardsFoldPolicyConfig lCfg)
    , rewardsFoldValidatorHash = R.rewardsFoldScriptValidatorHash (rewardsFoldConfig lCfg)
    , commitFoldSymbol = C.commitFoldMintingPolicySymbol (commitFoldPolicyConfig lCfg)
    , commitFoldValidatorHash = C.commitFoldScriptValidatorHash (commitFoldConfig lCfg)
    , tokensHolderSymbol = PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig lCfg)
    , tokensHolderValidatorHash = PTHFirst.projectTokensHolderScriptValidatorHash (firstTokensHolderConfig lCfg)
    , failProofSymbol = FP.failProofPolicySymbol
    , failProofValidatorHash = FP.failProofScriptValidatorHash
    , defaultTierMinCommitment
    , defaultTierMaxCommitment
    , presaleTierCs
    , presaleTierMinCommitment
    , presaleTierMaxCommitment
    , presaleTierStartTime
    , defaultStartTime
    , startTime = lCfg.startTime
    , endTime
    , projectMinCommitment
    , projectMaxCommitment
    , raisingSymbol
    , raisingToken = rToken
    , owner
    , daoAdmin
    , totalTokens
    , daoFeeReceiver
    , projectSymbol
    , projectToken = pToken
    , collateral
    , nodeAda
    , oilAda
    , commitFoldFeeAda
    }
  where
    AssetClass (raisingSymbol, rToken) = raisingToken
    AssetClass (projectSymbol, pToken) = projectToken

commitFoldConfig :: LaunchpadConfig -> C.CommitFoldConfig
commitFoldConfig lCfg =
  C.CommitFoldConfig
    { starter = lCfg.starter
    , commitFoldSymbol = C.commitFoldMintingPolicySymbol (commitFoldPolicyConfig lCfg)
    , nodeSymbol = N.nodeMintingPolicySymbol (nodePolicyConfig lCfg)
    , endTime = lCfg.endTime
    , daoAdmin = lCfg.daoAdmin
    }

nodePolicyConfig :: LaunchpadConfig -> N.NodePolicyConfig
nodePolicyConfig lCfg =
  N.NodePolicyConfig
    { starter = lCfg.starter
    , owner = addressToPubKeyHash lCfg.owner
    , nodeAda = lCfg.nodeAda
    }

commitFoldPolicyConfig :: LaunchpadConfig -> C.CommitFoldPolicyConfig
commitFoldPolicyConfig lCfg =
  C.CommitFoldPolicyConfig
    { starter = lCfg.starter
    , endTime = lCfg.endTime
    , nodeSymbol = N.nodeMintingPolicySymbol (nodePolicyConfig lCfg)
    }

rewardsFoldConfig :: LaunchpadConfig -> R.RewardsFoldConfig
rewardsFoldConfig lCfg =
  R.RewardsFoldConfig
    { starter = lCfg.starter
    , nodeSymbol = N.nodeMintingPolicySymbol (nodePolicyConfig lCfg)
    , rewardsFoldPolicy = R.rewardsFoldMintingPolicySymbol (rewardsFoldPolicyConfig lCfg)
    , rewardsHolderValidatorHash = RH.rewardsHolderScriptValidatorHash (rewardsHolderConfig lCfg)
    , finalProjectTokensHolderValidatorHash = PTHF.projectTokensHolderScriptValidatorHash (finalTokensHolderConfig lCfg)
    , firstProjectTokensHolderValidatorHash = PTHFirst.projectTokensHolderScriptValidatorHash (firstTokensHolderConfig lCfg)
    , projectTokensHolderPolicy = PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig lCfg)
    , projectSymbol = fst (unAssetClass lCfg.projectToken)
    , projectToken = snd (unAssetClass lCfg.projectToken)
    , raisingSymbol = fst (unAssetClass lCfg.raisingToken)
    , raisingToken = snd (unAssetClass lCfg.raisingToken)
    , presaleTierCs = lCfg.presaleTierCs
    , tokensToDistribute = lCfg.tokensToDistribute
    , endTime = lCfg.endTime
    , oilAda = lCfg.oilAda
    , commitFoldFeeAda = lCfg.commitFoldFeeAda
    , splitBps = lCfg.splitBps
    , owner = lCfg.owner
    , daoFeeNumerator = lCfg.daoFeeNumerator
    , daoFeeDenominator = lCfg.daoFeeDenominator
    , daoFeeReceiver = lCfg.daoFeeReceiver
    , collateral = lCfg.collateral
    , raisedTokensPoolPartPercentage = lCfg.raisedTokensPoolPartPercentage
    }

rewardsFoldPolicyConfig :: LaunchpadConfig -> R.RewardsFoldPolicyConfig
rewardsFoldPolicyConfig lCfg = lCfg.starter

tokensHolderPolicyConfig :: LaunchpadConfig -> PTH.TokensHolderPolicyConfig
tokensHolderPolicyConfig lCfg =
  PTH.TokensHolderPolicyConfig
    { starter = lCfg.starter
    , owner = addressToPubKeyHash lCfg.owner
    , startTime = lCfg.startTime
    , totalTokens = lCfg.totalTokens
    , projectSymbol = fst (unAssetClass lCfg.projectToken)
    , projectToken = snd (unAssetClass lCfg.projectToken)
    , nodeSymbol = N.nodeMintingPolicySymbol (nodePolicyConfig lCfg)
    , collateral = lCfg.collateral
    }

firstTokensHolderConfig :: LaunchpadConfig -> PTHFirst.TokensHolderFirstConfig
firstTokensHolderConfig lCfg =
  PTHFirst.TokensHolderFirstConfig
    { starter = lCfg.starter
    , owner = lCfg.owner
    , projectTokensHolderSymbol = PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig lCfg)
    , startTime = lCfg.startTime
    , endTime = lCfg.endTime
    , daoAdmin = lCfg.daoAdmin
    }

finalTokensHolderConfig :: LaunchpadConfig -> PTHF.TokensHolderFinalConfig
finalTokensHolderConfig lCfg =
  PTHF.TokensHolderFinalConfig
    { starter = lCfg.starter
    , owner = lCfg.owner
    , wrPoolSymbol = lCfg.wrPoolCurrencySymbol
    , wrPoolValidatorHash = lCfg.wrPoolValidatorHash
    , wrFactoryValidatorHash = lCfg.wrFactoryValidatorHash
    , sundaePoolScriptHash = lCfg.sundaePoolScriptHash
    , sundaeFeeTolerance = lCfg.sundaeFeeTolerance
    , sundaeSettingsCurrencySymbol = lCfg.sundaeSettingsCurrencySymbol
    , vestingValidatorHash = lCfg.vestingValidatorHash
    , vestingPeriodDuration = lCfg.vestingPeriodDuration
    , vestingPeriodDurationToFirstUnlock = lCfg.vestingPeriodDurationToFirstUnlock
    , vestingPeriodInstallments = lCfg.vestingPeriodInstallments
    , vestingPeriodStart = lCfg.vestingPeriodStart
    , daoFeeReceiver = lCfg.daoFeeReceiver
    , raisingSymbol = fst (unAssetClass lCfg.raisingToken)
    , raisingToken = snd (unAssetClass lCfg.raisingToken)
    , projectSymbol = fst (unAssetClass lCfg.projectToken)
    , projectToken = snd (unAssetClass lCfg.projectToken)
    , poolProofValidatorHash = poolProofScriptValidatorHash (poolProofConfig lCfg)
    }

poolProofPolicyConfig :: LaunchpadConfig -> PoolProofPolicyConfig
poolProofPolicyConfig lCfg =
  PoolProofPolicyConfig
    { wrPoolValidatorHash = lCfg.wrPoolValidatorHash
    , wrPoolSymbol = lCfg.wrPoolCurrencySymbol
    , sundaePoolScriptHash = lCfg.sundaePoolScriptHash
    }

poolProofConfig :: LaunchpadConfig -> PoolProofConfig
poolProofConfig lCfg = poolProofMintingPolicySymbol (poolProofPolicyConfig lCfg)

rewardsHolderConfig :: LaunchpadConfig -> RH.RewardsHolderConfig
rewardsHolderConfig lCfg =
  RH.RewardsHolderConfig
    { poolProofValidatorHash = poolProofScriptValidatorHash (poolProofConfig lCfg)
    , poolProofSymbol = poolProofMintingPolicySymbol (poolProofPolicyConfig lCfg)
    }
