module Integration.Launchpad.ProjectTokensHolderFinal where

import Control.Monad (when)
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util
import Integration.Vesting (vestingValidator)
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes (SundaePoolDatum, SundaeSettingsDatum (..), WrPoolConstantProductDatum (..))
import Launchpad.ProjectTokensHolderFinal qualified as PTHF
import Launchpad.Types (Dex (..))
import Other.Vesting (VestingDatum (..))
import Plutus.Model
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value (assetClassValue, assetClassValueOf)
import PlutusLedgerApi.V2 (CurrencySymbol (..), PubKeyHash, singleton, toBuiltinData)
import PlutusTx.Prelude (inv)
import Test.Util (vUSDT)
import Unit.Launchpad.UtilFunctions (unwrapScriptHash)

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

createProjectTokensHolderFinal :: LaunchpadConfig -> Dex -> Integer -> PubKeyHash -> Run ()
createProjectTokensHolderFinal
  config@LaunchpadConfig {projectToken, raisingToken, totalTokens, tokensToDistribute}
  dex
  raised
  wallet = do
    let value =
          assetClassValue projectToken (totalTokens - tokensToDistribute)
            <> assetClassValue raisingToken raised
    -- Incorrect, but we must have at least something here, like 2 ada?
    -- <> assetClassValue adaAssetClass config.collateral

    usp <- spend wallet value
    let tx =
          mconcat
            [ userSpend usp
            , payToScript (projectTokensHolderFinalValidator config) (InlineDatum dex) value
            ]
    ensureTx wallet tx

mockSettingsDatum :: Integer -> SundaeSettingsDatum
mockSettingsDatum poolCreationFee =
  SundaeSettingsDatum
    { _settingsAdmin = toBuiltinData ()
    , _metadataAdmin = toBuiltinData ()
    , _treasuryAdmin = toBuiltinData ()
    , _treasuryAddress = toBuiltinData ()
    , _treasuryAllowance = toBuiltinData ()
    , _authorizedScoopers = toBuiltinData ()
    , _authorizedStakingKeys = toBuiltinData ()
    , _baseFee = toBuiltinData ()
    , _simpleFee = toBuiltinData ()
    , _strategyFee = toBuiltinData ()
    , poolCreationFee
    , _extensions = toBuiltinData ()
    }

maxShareTokens :: Num a => a
maxShareTokens = 9_223_372_036_854_775_807

spendHolderCreatePool :: MaliciousTokensHolderAction -> Dex -> LaunchpadConfig -> PubKeyHash -> PubKeyHash -> Run ()
spendHolderCreatePool action dex config@LaunchpadConfig {..} wallet signer = do
  let shareQuantity p r = floor @Double (sqrt (fromIntegral (p * r)))

  -- create a mock factory for Wingriders
  when (dex == Wr) $ do
    ensureTx wallet $ payToScript mockFactoryScript (InlineDatum ()) mempty
  factories <- utxoAt mockFactoryScript

  -- create a settins utxo for Sundae
  when (dex == Sundae) $ do
    let value = adaValue 2_000_000 <> singleton mockSundaeSettingsCurrencySymbol C.settingsNftName 1
    usp <- spend wallet value
    ensureTx wallet $
      userSpend usp <> payToScript mockSundaeSettingsScript (InlineDatum (mockSettingsDatum sundaeFee)) value
  settings <- utxoAt mockSundaeSettingsScript

  holderUtxos <- boxAt (projectTokensHolderFinalValidator config)
  let holderUtxo = case filter ((dex ==) . txBoxDatum) holderUtxos of
        h : _ -> h
        [] -> error "spendHolderCreatePool: no dex holder utxos"

      holderValue = txBoxValue holderUtxo
      projectTokensQty = assetClassValueOf holderValue projectToken
      raisedTokensQty = assetClassValueOf holderValue raisingToken

      lpShareTn = case dex of
        Wr ->
          if raisingToken < projectToken
            then wrShareTokenName raisingToken projectToken
            else wrShareTokenName projectToken raisingToken
        Sundae -> poolSundaeLpName mockSundaeIdentifier

      lpShareCs = case dex of
        Wr -> wrPoolCurrencySymbol
        Sundae -> CurrencySymbol (unwrapScriptHash sundaePoolScriptHash)

      vestingDatum =
        VestingDatum
          { beneficiary = case action of
              WrongBeneficiary -> pubKeyHashAddress maliciousPkh
              _ -> owner
          , vestingSymbol = case action of
              WrongVestingAsset -> adaSymbol
              _ -> lpShareCs
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

      nftTn = case dex of
        Wr -> C.wrLpValidityTokenName
        Sundae -> poolSundaeNftName mockSundaeIdentifier
      poolToken = case dex of
        Wr -> singleton wrPoolCurrencySymbol C.wrLpValidityTokenName 1
        Sundae -> singleton lpShareCs (poolSundaeNftName mockSundaeIdentifier) 1

      mintedShares = shareQuantity projectTokensQty raisedTokensQty

      poolShares = case dex of
        Wr -> singleton lpShareCs lpShareTn (maxShareTokens - mintedShares)
        Sundae -> mempty

      mintedValue =
        -- NOTE: there are 3 values for Sundae in a genuine transaction
        singleton lpShareCs nftTn 1
          <> singleton lpShareCs lpShareTn mintedShares

      vestingValue =
        singleton lpShareCs lpShareTn vestingDatum.totalVestingQty <> case action of
          WrongVestingQuantity -> inv (singleton vestingDatum.vestingSymbol vestingDatum.vestingToken 1)
          MultipleTokenTypes -> assetClassValue vUSDT 1
          _ -> mempty

      -- NOTE: doesn't actually run the DEX-side validation
      --       also, for Sundae tests use a constant mocked pool identifier
      --       in reality it's computed from the first tx input out ref
      tx =
        let otherHolderUtxo = holderUtxos !! 1
         in mconcat
              [ userSpend usp
              , mintValue poolMintingPolicy () mintedValue
              , spendScript
                  (projectTokensHolderFinalValidator config)
                  (txBoxRef holderUtxo)
                  PTHF.NormalFlow
                  dex
              , case dex of
                  Wr ->
                    spendScript
                      mockFactoryScript
                      ( case factories of
                          [(ref, _)] -> ref
                          _ -> error "spendHolderCreatePool: no factory"
                      )
                      ()
                      ()
                  Sundae ->
                    refInputInline
                      ( case settings of
                          [(ref, _)] -> ref
                          _ -> error "spendHolderCreatePool: no settings"
                      )
              , case action of
                  DoubleSatisfy ->
                    spendScript
                      (projectTokensHolderFinalValidator config)
                      (txBoxRef otherHolderUtxo)
                      PTHF.NormalFlow
                      dex
                  _ -> mempty
              , payToScript vestingValidator (InlineDatum vestingDatum) vestingValue
              , -- NOTE: we use the same "free" policy for both Sundae and Wr in testing
                mintValue poolMintingPolicy () poolShares
              , case dex of
                  Wr ->
                    payToScript
                      (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash))
                      (InlineDatum (wrDatum projectToken raisingToken))
                      (poolValue <> poolToken <> poolShares)
                  Sundae ->
                    payToScript
                      (TypedValidatorHash @SundaePoolDatum (toV2 sundaePoolScriptHash))
                      (InlineDatum (sundaeDatum mockSundaeIdentifier projectToken raisingToken mintedShares sundaeFee))
                      (poolValue <> poolToken <> poolShares)
              ]

  lower <- actualTime
  let upper = lower + 1_000
  submitTx wallet =<< validateIn (interval lower upper) =<< signTx signer tx

spendHolderFundsToDao :: MaliciousTokensHolderAction -> LaunchpadConfig -> Dex -> PubKeyHash -> PubKeyHash -> Run ()
spendHolderFundsToDao action config@LaunchpadConfig {..} dex wallet signer = do
  [holderUtxo] <- boxAt (projectTokensHolderFinalValidator config)

  let projectTokensQty = assetClassValueOf (txBoxValue holderUtxo) projectToken
      raisedTokensQty = assetClassValueOf (txBoxValue holderUtxo) raisingToken

      daoFeeReceiverValue =
        assetClassValue raisingToken raisedTokensQty
          <> assetClassValue projectToken projectTokensQty
          <> case action of
            NoProjectTokensToDao -> inv (assetClassValue projectToken projectTokensQty)
            LessProjectTokensToDao -> inv (assetClassValue projectToken 1)
            _ -> mempty

  tx <-
    if (dex == Wr)
      then do
        [poolProofUtxo] <- boxAt (poolProofValidator config)
        pure . mconcat $
          [ case action of
              NoPoolProof -> payToKey daoFeeReceiver mempty
              _ -> refInputInline (txBoxRef poolProofUtxo)
          , spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.FailedFlow dex
          , payToKey daoFeeReceiver daoFeeReceiverValue
          ]
      else do
        let value = adaValue 2_000_000 <> singleton mockSundaeSettingsCurrencySymbol C.settingsNftName 1
        usp <- spend wallet value
        ensureTx wallet $
          userSpend usp <> payToScript mockSundaeSettingsScript (InlineDatum (mockSettingsDatum sundaeFee)) value
        [(settings, _)] <- utxoAt mockSundaeSettingsScript

        pure . mconcat $
          [ refInputInline settings
          , spendScript (projectTokensHolderFinalValidator config) (txBoxRef holderUtxo) PTHF.FailedFlow dex
          , payToKey daoFeeReceiver daoFeeReceiverValue
          ]

  submitTx wallet =<< signTx signer tx
