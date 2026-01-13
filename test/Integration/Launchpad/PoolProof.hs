module Integration.Launchpad.PoolProof where

import Data.Functor (void)
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util (ensureTx, mockSundaeIdentifier, poolSundaeNftName)
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Launchpad.Types (Dex (..), PoolProofDatum (..))
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClass,
  assetClassValue,
  singleton,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  PubKeyHash,
  TxOutRef,
 )
import Test.Util (vETH)
import Unit.Launchpad.UtilFunctions (unwrapScriptHash)

data MaliciousPoolProofAction
  = None
  | WrongTokenName
  | MintedTwoTokens
  | MintedDifferentTokens
  | IncorrectPoolHash
  | DifferentProjectToken
  | NoProofValidityToken

maliciousScriptHash :: ScriptHash
maliciousScriptHash = "36bea2acff0a1c9376b0fd4137ee46fb0f7acfd173ec071e338f8001"

maliciousPkh :: PubKeyHash
maliciousPkh = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

spendTwoPoolProofs :: LaunchpadConfig -> PubKeyHash -> Run ()
spendTwoPoolProofs config wallet = do
  utxos <- boxAt (poolProofValidator config)
  tx <- signTx wallet $ spendTwoPoolProofsTx config utxos wallet
  void $ sendTx tx

spendTwoPoolProofsTx :: LaunchpadConfig -> [TxBox (TypedValidator PoolProofDatum ())] -> PubKeyHash -> Tx
spendTwoPoolProofsTx config txBoxes wallet =
  mconcat
    [ spendScript (poolProofValidator config) (txBoxRef first) () (txBoxDatum first)
    , payToScript (poolProofValidator config) (InlineDatum $ txBoxDatum first) tokenValue
    , spendScript (poolProofValidator config) (txBoxRef second) () (txBoxDatum second)
    , payToKey wallet tokenValue
    ]
  where
    first = txBoxes !! 0
    second = txBoxes !! 1
    tokenValue =
      singleton
        ( scriptCurrencySymbol
            (poolProofMintingPolicy config)
        )
        (scriptHashToTokenName (toValidatorHash (poolProofValidator config)))
        1

spendPoolProof :: LaunchpadConfig -> PubKeyHash -> Run ()
spendPoolProof config wallet = do
  [utxo] <- boxAt (poolProofValidator config)
  tx <-
    signTx wallet $
      mconcat
        [ spendBox (poolProofValidator config) () utxo
        , payToScript (poolProofValidator config) (InlineDatum $ txBoxDatum utxo) (txBoxValue utxo)
        ]
  void $ sendTx tx

createPoolProof :: MaliciousPoolProofAction -> Dex -> LaunchpadConfig -> PubKeyHash -> Run ()
createPoolProof action dex config@LaunchpadConfig {wrPoolValidatorHash, sundaePoolScriptHash} wallet = do
  utxos <- case (dex, action) of
    (Wr, IncorrectPoolHash) -> utxoAt (TypedValidatorHash @WrPoolConstantProductDatum (toV2 maliciousScriptHash))
    (Wr, _) -> utxoAt (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash))
    (Sundae, IncorrectPoolHash) -> utxoAt (TypedValidatorHash @SundaePoolDatum (toV2 maliciousScriptHash))
    (Sundae, _) -> utxoAt (TypedValidatorHash @SundaePoolDatum (toV2 sundaePoolScriptHash))
  let (txOutRef, _) = case utxos of
        h : _ -> h
        [] -> error "createPoolProof: no pool utxos"

  tx <- signTx wallet $ createPoolProofTx action dex config txOutRef
  void $ sendTx tx

createPoolProofTx :: MaliciousPoolProofAction -> Dex -> LaunchpadConfig -> TxOutRef -> Tx
createPoolProofTx action dex config poolRef =
  mconcat
    [ case action of
        NoProofValidityToken -> mempty
        _ -> mintValue (poolProofMintingPolicy config) dex mintedValue
    , case dex of
        Wr -> refInputHash poolRef (wrDatum config.projectToken config.raisingToken)
        Sundae -> refInputInline poolRef
    , payToScript (poolProofValidator config) (InlineDatum poolProofDatum) mintedValue
    ]
  where
    mintedValue = case action of
      WrongTokenName ->
        singleton
          ( scriptCurrencySymbol (poolProofMintingPolicy config)
          )
          "NotScriptHash"
          1
      MintedTwoTokens ->
        singleton
          ( scriptCurrencySymbol (poolProofMintingPolicy config)
          )
          (scriptHashToTokenName (toValidatorHash (poolProofValidator config)))
          2
      MintedDifferentTokens ->
        singleton
          ( scriptCurrencySymbol (poolProofMintingPolicy config)
          )
          (scriptHashToTokenName (toValidatorHash (poolProofValidator config)))
          1
          <> singleton
            ( scriptCurrencySymbol (poolProofMintingPolicy config)
            )
            "NotScriptHash"
            1
      NoProofValidityToken -> mempty
      _ ->
        singleton
          ( scriptCurrencySymbol
              (poolProofMintingPolicy config)
          )
          (scriptHashToTokenName (toValidatorHash (poolProofValidator config)))
          1
    poolProofDatum = PoolProofDatum projectSymbol projectToken raisingSymbol raisingToken dex
    AssetClass (projectSymbol, projectToken) = config.projectToken
    AssetClass (raisingSymbol, raisingToken) = config.raisingToken

poolOilAda :: Num a => a
poolOilAda = 3_000_000

createPoolUtxo :: MaliciousPoolProofAction -> Dex -> LaunchpadConfig -> PubKeyHash -> Run ()
createPoolUtxo
  action
  dex
  LaunchpadConfig
    { raisingToken
    , projectToken
    , wrPoolCurrencySymbol
    , wrPoolValidatorHash
    , sundaePoolScriptHash
    , sundaeFee
    }
  wallet = do
    let identifier = mockSundaeIdentifier
        value = case dex of
          Wr ->
            assetClassValue adaAssetClass poolOilAda
              <> assetClassValue raisingToken 10_000
              <> assetClassValue (assetClass wrPoolCurrencySymbol "lpShare") 1_000
              <> assetClassValue (assetClass wrPoolCurrencySymbol C.wrLpValidityTokenName) 1
              <> assetClassValue projectToken 10_000
          Sundae ->
            assetClassValue adaAssetClass poolOilAda
              <> assetClassValue raisingToken 10_000
              <> singleton (CurrencySymbol (unwrapScriptHash sundaePoolScriptHash)) (poolSundaeNftName identifier) 1
              <> assetClassValue projectToken 10_000
        datumForWr = case action of
          DifferentProjectToken -> wrDatum vETH raisingToken
          _ -> wrDatum projectToken raisingToken
        datumForSundae = case action of
          DifferentProjectToken -> sundaeDatum identifier vETH raisingToken 1_000_000
          _ -> sundaeDatum identifier projectToken raisingToken 1_000_000

    usp <- spend wallet value

    ensureTx wallet $
      mconcat
        [ userSpend usp
        , case (dex, action) of
            (Wr, IncorrectPoolHash) ->
              payToScript
                (TypedValidatorHash @WrPoolConstantProductDatum (toV2 maliciousScriptHash))
                (InlineDatum datumForWr)
                value
            (Sundae, IncorrectPoolHash) ->
              payToScript
                (TypedValidatorHash @SundaePoolDatum (toV2 maliciousScriptHash))
                (InlineDatum (datumForSundae sundaeFee))
                value
            (Wr, _) ->
              payToScript
                (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash))
                (InlineDatum datumForWr)
                value
            (Sundae, _) ->
              payToScript
                (TypedValidatorHash @SundaePoolDatum (toV2 sundaePoolScriptHash))
                (InlineDatum (datumForSundae sundaeFee))
                value
        ]
