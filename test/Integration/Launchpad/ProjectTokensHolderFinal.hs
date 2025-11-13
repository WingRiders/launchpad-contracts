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
  | NoPoolProof
  | LessProjectTokensToDao
  | NoProjectTokensToDao
  | WrongPoolProof
  | DoubleSatisfy

maliciousPkh :: PubKeyHash
maliciousPkh = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

createProjectTokensHolderFinal :: LaunchpadConfig -> Integer -> PubKeyHash -> Run ()
createProjectTokensHolderFinal config@LaunchpadConfig {projectToken, raisingToken, totalTokens, tokensToDistribute} raised wallet = do
  let value =
        assetClassValue projectToken (totalTokens - tokensToDistribute)
          <> assetClassValue raisingToken raised
  -- Incorrect, but we must have at least something here, like 2 ada
  -- <> assetClassValue adaAssetClass config.collateral

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
  let shareQuantity p r = floor @Double (sqrt (fromIntegral (p * r)))

  submitTx wallet createMockFactoryTx
  [(factoryRef, _)] <- utxoAt mockFactoryScript

  holderUtxos <- boxAt (projectTokensHolderFinalValidator config)
  -- TODO: allow for two holders
  let holderUtxo = head holderUtxos
      holderValue = txBoxValue holderUtxo

  lower <- actualTime
  let upper = lower + 1_000

      projectTokensQty = assetClassValueOf holderValue projectToken
      raisedTokensQty = assetClassValueOf holderValue raisingToken

      lpShareTn = if raisingToken < projectToken then shareTokenName raisingToken projectToken else shareTokenName projectToken raisingToken

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
              WrongVestingQuantity -> shareQuantity projectTokensQty raisedTokensQty + 1
              _ -> shareQuantity projectTokensQty raisedTokensQty
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

  let poolValue =
        assetClassValue projectToken projectTokensQty
          <> assetClassValue raisingToken raisedTokensQty
      poolToken = singleton wrPoolCurrencySymbol C.lpValidityTokenName 1

      mintedShares = shareQuantity projectTokensQty raisedTokensQty

      poolShares = singleton wrPoolCurrencySymbol lpShareTn (C.maxShareTokens - mintedShares)
      mintedValue =
        singleton wrPoolCurrencySymbol C.lpValidityTokenName 1
          <> singleton wrPoolCurrencySymbol lpShareTn mintedShares

      vestingValue =
        singleton wrPoolCurrencySymbol lpShareTn vestingDatum.totalVestingQty <> case action of
          WrongVestingQuantity -> inv (singleton vestingDatum.vestingSymbol vestingDatum.vestingToken 1)
          MultipleTokenTypes -> assetClassValue vUSDT 1
          _ -> mempty
      tx =
        spendHolderCreatePoolTx
          action
          config
          usp
          factoryRef
          holderUtxos
          vestingDatum
          mintedValue
          poolValue
          poolToken
          poolShares
          vestingValue

  submitTx wallet =<< validateIn (interval lower upper) =<< signTx signer tx

createMockFactoryTx :: Tx
createMockFactoryTx = mconcat [payToScript mockFactoryScript (InlineDatum ()) mempty]

spendHolderCreatePoolTx ::
  MaliciousTokensHolderAction ->
  LaunchpadConfig ->
  UserSpend ->
  TxOutRef ->
  [TxBox (TypedValidator Dex PTHF.TokensHolderFinalRedeemer)] ->
  VestingDatum ->
  Value ->
  Value ->
  Value ->
  Value ->
  Value ->
  Tx
spendHolderCreatePoolTx
  action
  config@LaunchpadConfig {wrPoolValidatorHash, projectToken, raisingToken}
  usp
  mockFactoryRef
  holderUtxos
  vestingDatum
  mintedValue
  poolValue
  poolToken
  poolShares
  vestingValue = do
    let holderUtxo = head holderUtxos
        otherHolderUtxo = holderUtxos !! 1

    -- NOTE: doesn't actually run the DEX-side validation
    mconcat
      [ userSpend usp
      , mintValue poolMintingPolicy () mintedValue
      , spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.NoPool Wr
      , spendScript mockFactoryScript mockFactoryRef () ()
      , case action of
          DoubleSatisfy -> spendScript (projectTokensHolderFinalValidator config) (txBoxRef otherHolderUtxo) PTHF.NormalFlow Wr
          _ -> spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.NoPool Wr
      , payToScript vestingValidator (InlineDatum vestingDatum) vestingValue
      , mintValue (poolMintingPolicy) () poolShares
      , payToScript (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash)) (InlineDatum (lpDatum projectToken raisingToken)) (poolValue <> poolToken <> poolShares)
      ]

spendHolderPoolExists :: MaliciousTokensHolderAction -> LaunchpadConfig -> PubKeyHash -> PubKeyHash -> Run ()
spendHolderPoolExists action config@LaunchpadConfig {..} wallet signer = do
  [holderUtxo] <- boxAt (projectTokensHolderFinalValidator config)
  [poolProofUtxo] <- boxAt (poolProofValidator config)

  let projectTokensQty = assetClassValueOf (txBoxValue holderUtxo) projectToken
      raisedTokensQty = assetClassValueOf (txBoxValue holderUtxo) raisingToken

      daoFeeReceiverValue =
        assetClassValue raisingToken raisedTokensQty
          <> assetClassValue projectToken projectTokensQty
          <> case action of
            NoProjectTokensToDao -> inv (assetClassValue projectToken projectTokensQty)
            LessProjectTokensToDao -> inv (assetClassValue projectToken 1)
            _ -> mempty

  submitTx wallet =<< signTx signer (spendHolderPoolExistsTx action config holderUtxo poolProofUtxo daoFeeReceiverValue)

spendHolderPoolExistsTx :: MaliciousTokensHolderAction -> LaunchpadConfig -> TxBox (TypedValidator Dex PTHF.TokensHolderFinalRedeemer) -> TxBox (TypedValidator PoolProofDatum ()) -> Value -> Tx
spendHolderPoolExistsTx action config@LaunchpadConfig {daoFeeReceiver} holderUtxo poolProofUtxo daoFeeReceiverValue =
  mconcat
    [ case action of
        NoPoolProof -> payToKey daoFeeReceiver mempty
        _ -> refInputHash (txBoxRef poolProofUtxo) (txBoxDatum poolProofUtxo) -- TODO: doesn't that work?
    , spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.FailedFlow Wr
    , payToKey daoFeeReceiver daoFeeReceiverValue
    ]
