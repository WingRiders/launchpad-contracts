module Integration.Launchpad.PoolProof where

import Data.Functor (void)
import Integration.Launchpad.Validators
import Integration.Mock
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Launchpad.Types (PoolProofDatum (..))
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
  PubKeyHash,
  TxOutRef,
  Value,
 )
import Test.Util (vETH)

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

createPoolProof :: MaliciousPoolProofAction -> LaunchpadConfig -> PubKeyHash -> Run ()
createPoolProof action config@LaunchpadConfig {wrPoolValidatorHash} wallet = do
  utxos <- case action of
    IncorrectPoolHash -> utxoAt (TypedValidatorHash @WrPoolConstantProductDatum (toV2 maliciousScriptHash))
    _ -> utxoAt (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash))
  let (txOutRef, _) = head utxos

  tx <- signTx wallet $ createPoolProofTx action config txOutRef
  void $ sendTx tx

createPoolProofTx :: MaliciousPoolProofAction -> LaunchpadConfig -> TxOutRef -> Tx
createPoolProofTx action config poolWrRef =
  mconcat
    [ case action of
        NoProofValidityToken -> mempty
        _ -> mintValue (poolProofMintingPolicy config) 0 mintedValue
    , refInputHash poolWrRef (lpDatum config.projectToken config.raisingToken)
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
    poolProofDatum = PoolProofDatum projectSymbol projectToken raisingSymbol raisingToken 0
    AssetClass (projectSymbol, projectToken) = config.projectToken
    AssetClass (raisingSymbol, raisingToken) = config.raisingToken

createWrPoolUTxO :: MaliciousPoolProofAction -> LaunchpadConfig -> PubKeyHash -> Run ()
createWrPoolUTxO action LaunchpadConfig {projectToken, raisingToken, wrPoolValidatorHash, wrPoolCurrencySymbol} wallet = do
  let lpValue =
        assetClassValue adaAssetClass C.poolOilAda
          <> assetClassValue raisingToken 10_000
          <> assetClassValue (assetClass wrPoolCurrencySymbol "lpShare") 1_000
          <> assetClassValue (assetClass wrPoolCurrencySymbol C.lpValidityTokenName) 1
          <> assetClassValue projectToken 10_000

  let poolDatum = case action of
        DifferentProjectToken -> lpDatum vETH raisingToken
        _ -> lpDatum projectToken raisingToken

  usp <- spend wallet lpValue
  tx <- signTx wallet $ createWrPoolTx action usp wrPoolValidatorHash poolDatum lpValue
  void $ sendTx tx

createWrPoolTx :: MaliciousPoolProofAction -> UserSpend -> ScriptHash -> WrPoolConstantProductDatum -> Value -> Tx
createWrPoolTx action usp wrPoolValidatorHash poolDatum val =
  mconcat
    [ userSpend usp
    , case action of
        IncorrectPoolHash -> payToScript (TypedValidatorHash @WrPoolConstantProductDatum (toV2 maliciousScriptHash)) (InlineDatum poolDatum) val
        _ -> payToScript (TypedValidatorHash @WrPoolConstantProductDatum (toV2 wrPoolValidatorHash)) (InlineDatum poolDatum) val
    ]
