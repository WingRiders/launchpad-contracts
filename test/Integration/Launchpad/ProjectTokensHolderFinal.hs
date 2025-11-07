module Integration.Launchpad.ProjectTokensHolderFinal where

import Control.Monad (void)
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util
import Integration.Vesting (vestingValidator)
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes (WrPoolConstantProductDatum (..))
import Launchpad.ProjectTokensHolderFinal qualified as PTHF
import Launchpad.Types (Dex (..), PoolProofDatum (..))
import Other.Vesting (VestingDatum (..))
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value (assetClassValue, assetClassValueOf)
import PlutusLedgerApi.V2 (PubKeyHash, TxOutRef, Value, singleton)
import PlutusTx.Prelude (inv)
import Test.Util (vUSDT)

data MaliciousTokensHolderAction
  = None
  | WrongHashOrder
  | WrongBeneficiary
  | WrongVestingQuantity
  | WrongPeriodStart
  | WrongPeriodEnd
  | WrongFirstUnlock
  | WrongInstallments
  | WrongVestingAsset
  | MultipleTokenTypes
  | LessDaoFees
  | NoDaoFees
  | NoPoolProof
  | LessProjectTokensToDao
  | NoProjectTokensToDao
  | NoOwnerCompensations
  | WrongPoolProof
  | DoubleSatisfy

maliciousPkh :: PubKeyHash
maliciousPkh = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

createProjectTokensHolderFinal :: LaunchpadConfig -> Integer -> PubKeyHash -> Run ()
createProjectTokensHolderFinal config@LaunchpadConfig {projectToken, raisingToken, totalTokens, tokensToDistribute} raised wallet = do
  let value =
        assetClassValue projectToken (totalTokens - tokensToDistribute)
          <> assetClassValue raisingToken raised
          <> assetClassValue adaAssetClass config.collateral

  usp <- spend wallet value
  tx <- signTx wallet $ createProjectTokensHolderFinalTx config usp value
  void $ sendTx tx

createProjectTokensHolderFinalTx :: LaunchpadConfig -> UserSpend -> Value -> Tx
createProjectTokensHolderFinalTx config usp val =
  mconcat
    [ userSpend usp
    , payToScript (projectTokensHolderFinalValidator config) (InlineDatum Wr) val
    ]

spendHolderCreatePool :: MaliciousTokensHolderAction -> LaunchpadConfig -> PubKeyHash -> PubKeyHash -> Run ()
spendHolderCreatePool action config@LaunchpadConfig {..} wallet signer = do
  submitTx wallet createMockFactoryTx
  [(factoryRef, _)] <- utxoAt mockFactoryScript

  holderUtxos <- boxAt (projectTokensHolderFinalValidator config)
  let holderUtxo = head holderUtxos

  lower <- actualTime
  let upper = lower + 1_000

      projectTokensQty = assetClassValueOf (txBoxValue holderUtxo) projectToken
      raisedTokensQty =
        if raisingToken == adaAssetClass
          then assetClassValueOf (txBoxValue holderUtxo) raisingToken - config.collateral
          else assetClassValueOf (txBoxValue holderUtxo) raisingToken
      daoFee = div (raisedTokensQty * daoFeeUnits) daoFeeBase
      raisedTokensPoolPartQty = div ((raisedTokensQty - daoFee) * raisedTokensPoolPartPercentage) 100
      remainingRaisedTokensQty = raisedTokensQty - raisedTokensPoolPartQty - daoFee
      lpShareTn = case action of
        WrongHashOrder -> if raisingToken > projectToken then shareTokenName raisingToken projectToken else shareTokenName projectToken raisingToken
        _ -> if raisingToken < projectToken then shareTokenName raisingToken projectToken else shareTokenName projectToken raisingToken

      vestingDatum =
        VestingDatum
          { beneficiary = case action of
              WrongBeneficiary -> pubKeyHashAddress maliciousPkh
              _ -> owner
          , vestingSymbol = case action of
              WrongVestingAsset -> adaSymbol
              _ -> wrPoolCurrencySymbol
          , vestingToken = case action of
              WrongVestingAsset -> adaToken
              _ -> lpShareTn
          , totalVestingQty = case action of
              WrongVestingQuantity -> shareQuantity projectTokensQty raisedTokensPoolPartQty + 1
              _ -> shareQuantity projectTokensQty raisedTokensPoolPartQty
          , vestingPeriodStart = case action of
              WrongPeriodStart -> vestingPeriodStart + 1
              _ -> vestingPeriodStart
          , vestingPeriodEnd = case action of
              WrongPeriodEnd -> vestingPeriodStart + vestingPeriodDuration + 1
              _ -> vestingPeriodStart + vestingPeriodDuration
          , firstUnlockPossibleAfter = case action of
              WrongFirstUnlock -> vestingPeriodStart + vestingPeriodDurationToFirstUnlock + 1
              _ -> vestingPeriodStart + vestingPeriodDurationToFirstUnlock
          , totalInstallments = case action of
              WrongInstallments -> vestingPeriodInstallments + 1
              _ -> vestingPeriodInstallments
          , vestingMemo = "0123"
          }

  usp <- case action of
    MultipleTokenTypes -> spend wallet (assetClassValue vUSDT 1)
    _ -> spend wallet mempty

  let poolValue = assetClassValue projectToken projectTokensQty <> assetClassValue raisingToken raisedTokensPoolPartQty
      poolToken = singleton wrPoolCurrencySymbol C.lpValidityTokenName 1
      poolShares = singleton wrPoolCurrencySymbol lpShareTn (C.maxShareTokens - (shareQuantity projectTokensQty raisedTokensPoolPartQty))
      daoFeeReceiverValue = case action of
        NoOwnerCompensations -> assetClassValue raisingToken (remainingRaisedTokensQty + daoFee) <> assetClassValue adaAssetClass config.collateral
        _ -> assetClassValue raisingToken daoFee
      mintedValue = singleton wrPoolCurrencySymbol C.lpValidityTokenName 1 <> singleton wrPoolCurrencySymbol lpShareTn (shareQuantity projectTokensQty raisedTokensPoolPartQty)
      vestingValue =
        singleton wrPoolCurrencySymbol lpShareTn vestingDatum.totalVestingQty <> case action of
          WrongVestingQuantity -> inv (singleton vestingDatum.vestingSymbol vestingDatum.vestingToken 1)
          MultipleTokenTypes -> assetClassValue vUSDT 1
          _ -> mempty
      ownerValue =
        case action of
          NoOwnerCompensations -> mempty
          _ -> assetClassValue raisingToken remainingRaisedTokensQty <> assetClassValue adaAssetClass config.collateral
      tx = spendHolderCreatePoolTx action config usp factoryRef holderUtxos vestingDatum mintedValue poolValue poolToken poolShares vestingValue ownerValue daoFeeReceiverValue

  submitTx wallet =<< validateIn (interval lower upper) =<< signTx signer tx
  where
    shareQuantity :: Integer -> Integer -> Integer
    shareQuantity p r = floor @Double (sqrt (fromIntegral (p * r)))

createMockFactoryTx :: Tx
createMockFactoryTx = mconcat [payToScript mockFactoryScript (InlineDatum ()) mempty]

spendHolderCreatePoolTx :: MaliciousTokensHolderAction -> LaunchpadConfig -> UserSpend -> TxOutRef -> [TxBox (TypedValidator Dex PTHF.TokensHolderFinalRedeemer)] -> VestingDatum -> Value -> Value -> Value -> Value -> Value -> Value -> Value -> Tx
spendHolderCreatePoolTx action config@LaunchpadConfig {owner, daoFeeReceiver, wrPoolValidatorHash, projectToken, raisingToken} usp mockFactoryRef holderUtxos vestingDatum mintedValue poolValue poolToken poolShares vestingValue ownerValue daoFeeReceiverValue = do
  let holderUtxo = head holderUtxos
      otherHolderUtxo = holderUtxos !! 1
      stealValue =
        poolValue
          <> daoFeeReceiverValue
          <> ownerValue

  -- Correct transaction would also require spending and splitting Factory here (because of the pool creation)
  mconcat
    [ userSpend usp
    , mintValue poolMintingPolicy () mintedValue
    , spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.NoPool Wr
    , spendScript mockFactoryScript mockFactoryRef () ()
    , case action of
        DoubleSatisfy -> spendScript (projectTokensHolderFinalValidator config) (txBoxRef otherHolderUtxo) PTHF.NoPool Wr
        _ -> spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.NoPool Wr
    , payToScript vestingValidator (HashDatum vestingDatum) vestingValue
    , mintValue (poolMintingPolicy) () poolShares
    , payToScript (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash)) (InlineDatum (lpDatum projectToken raisingToken)) (poolValue <> poolToken <> poolShares)
    , case action of
        NoDaoFees -> payToKey daoFeeReceiver mempty
        LessDaoFees -> payToKey daoFeeReceiver (daoFeeReceiverValue <> inv (assetClassValue raisingToken 1))
        _ -> payToKey daoFeeReceiver daoFeeReceiverValue
    , case action of
        NoDaoFees -> payToKey owner (ownerValue <> daoFeeReceiverValue)
        LessDaoFees -> payToKey owner (ownerValue <> assetClassValue raisingToken 1)
        DoubleSatisfy -> payToKey owner (ownerValue <> stealValue)
        _ -> payToKey owner ownerValue
    ]

spendHolderPoolExists :: MaliciousTokensHolderAction -> LaunchpadConfig -> PubKeyHash -> PubKeyHash -> Run ()
spendHolderPoolExists action config@LaunchpadConfig {..} wallet signer = do
  [holderUtxo] <- boxAt (projectTokensHolderFinalValidator config)
  [poolProofUtxo] <- boxAt (poolProofValidator config)

  let projectTokensQty = assetClassValueOf (txBoxValue holderUtxo) projectToken
      raisedTokensQty =
        if raisingToken == adaAssetClass
          then assetClassValueOf (txBoxValue holderUtxo) raisingToken - config.collateral
          else assetClassValueOf (txBoxValue holderUtxo) raisingToken
      daoFee = div (raisedTokensQty * daoFeeUnits) daoFeeBase
      raisedTokensPoolPartQty = div ((raisedTokensQty - daoFee) * raisedTokensPoolPartPercentage) 100
      remainingRaisedTokensQty = raisedTokensQty - raisedTokensPoolPartQty - daoFee

      daoFeeReceiverValue =
        assetClassValue raisingToken daoFee
          <> assetClassValue projectToken projectTokensQty
          <> assetClassValue raisingToken raisedTokensPoolPartQty
          <> case action of
            NoProjectTokensToDao -> inv (assetClassValue projectToken projectTokensQty)
            LessProjectTokensToDao -> inv (assetClassValue projectToken 1)
            NoDaoFees -> inv (assetClassValue raisingToken daoFee)
            LessDaoFees -> inv (assetClassValue raisingToken 1)
            NoOwnerCompensations -> assetClassValue raisingToken remainingRaisedTokensQty <> assetClassValue adaAssetClass config.collateral
            _ -> mempty
      ownerValue =
        assetClassValue raisingToken remainingRaisedTokensQty
          <> assetClassValue adaAssetClass config.collateral
          <> case action of
            NoProjectTokensToDao -> assetClassValue projectToken projectTokensQty
            LessProjectTokensToDao -> assetClassValue projectToken 1
            NoDaoFees -> assetClassValue raisingToken daoFee
            LessDaoFees -> assetClassValue raisingToken 1
            NoOwnerCompensations -> inv (assetClassValue raisingToken remainingRaisedTokensQty <> assetClassValue adaAssetClass config.collateral)
            _ -> mempty

  submitTx wallet =<< signTx signer (spendHolderPoolExistsTx action config holderUtxo poolProofUtxo ownerValue daoFeeReceiverValue)

spendHolderPoolExistsTx :: MaliciousTokensHolderAction -> LaunchpadConfig -> TxBox (TypedValidator Dex PTHF.TokensHolderFinalRedeemer) -> TxBox (TypedValidator PoolProofDatum ()) -> Value -> Value -> Tx
spendHolderPoolExistsTx action config@LaunchpadConfig {owner, daoFeeReceiver} holderUtxo poolProofUtxo ownerValue daoFeeReceiverValue =
  mconcat
    [ case action of
        NoPoolProof -> payToKey owner mempty
        _ -> refInputHash (txBoxRef poolProofUtxo) (txBoxDatum poolProofUtxo)
    , spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.PoolExists Wr
    , payToKey daoFeeReceiver daoFeeReceiverValue
    , payToKey owner ownerValue
    ]
