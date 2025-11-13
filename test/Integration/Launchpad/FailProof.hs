module Integration.Launchpad.FailProof where

import Integration.Launchpad.Validators
import Integration.Mock
import Plutarch.Extra.ScriptContext
import Plutus.Model

import Data.Functor (void)
import Integration.Launchpad.Node (findNodeUtxo)
import Integration.Util (actualTime, addressToPubKeyHash, ensureTx, memptyIf, txOutputDatum)
import Launchpad.Types (
  CommitFoldDatum (..),
  CommitFoldRedeemer (..),
  LaunchpadTokensHolderDatum,
  LaunchpadTokensHolderFirstRedeemer (..),
  NodeRedeemer (..),
 )
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value (assetClassValue)
import PlutusLedgerApi.V1.Value qualified as V
import PlutusLedgerApi.V2
import Test.Util (vUSDT)

data MaliciousFailProofAction
  = None
  | NoBurningFold
  | NoBurningHolder
  | NoBurningNode
  | MultipleTokens

createFailProof :: LaunchpadConfig -> PubKeyHash -> Run ()
createFailProof config wallet = do
  let value =
        singleton
          (scriptCurrencySymbol (failProofMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (failProofValidator config)))
          1
  usp <- spend wallet value
  ensureTx wallet (createFailProofTx config usp value)

createFailProofTx :: LaunchpadConfig -> UserSpend -> Value -> Tx
createFailProofTx config usp value =
  mconcat
    [ userSpend usp
    , payToScript (failProofValidator config) (InlineDatum (toValidatorHash (nodeValidator config))) value
    ]

initFailProof :: MaliciousFailProofAction -> LaunchpadConfig -> PubKeyHash -> Run ()
initFailProof action config wallet = do
  now <- actualTime

  [(holderRef, _)] <- refScriptAt (projectTokensHolderFirstValidator config)
  [(nodeRef, _)] <- refScriptAt (nodeValidator config)
  [(commitFoldRef, _)] <- refScriptAt (commitFoldValidator config)

  [holderUtxo] <- boxAt (projectTokensHolderFirstValidator config)
  nodes <- utxoAt (nodeValidator config)
  let headNodeUtxo = findNodeUtxo Nothing nodes
  [commitFoldUtxo] <- boxAt (commitFoldValidator config)

  usp <- case action of
    MultipleTokens -> spend wallet (assetClassValue vUSDT 1)
    _ -> spend wallet mempty

  tx <- signTx wallet $ initFailProofTx action config usp (headNodeUtxo, nodeRef) (holderUtxo, holderRef) (commitFoldUtxo, commitFoldRef)
  void $ sendTx =<< validateIn (interval now (now + 1000)) tx

initFailProofTx ::
  MaliciousFailProofAction ->
  LaunchpadConfig ->
  UserSpend ->
  ((TxOutRef, TxOut), TxOutRef) ->
  (TxBox (TypedValidator LaunchpadTokensHolderDatum LaunchpadTokensHolderFirstRedeemer), TxOutRef) ->
  (TxBox (TypedValidator CommitFoldDatum CommitFoldRedeemer), TxOutRef) ->
  Tx
initFailProofTx action config usp ((nodeRef, nodeOut), nodeScriptRef) (holderUtxo, holderScriptRef) (commitFoldUtxo, commitFoldRef) =
  mconcat
    [ userSpend usp
    , spendScriptRef holderScriptRef (projectTokensHolderFirstValidator config) (txBoxRef holderUtxo) DelegateToRewardsOrFailure (txBoxDatum holderUtxo)
    , spendScriptRef nodeScriptRef (nodeValidator config) nodeRef FailLaunchpad (txOutputDatum nodeOut)
    , spendScriptRef commitFoldRef (commitFoldValidator config) (txBoxRef commitFoldUtxo) DelegateCommitToNode (txBoxDatum commitFoldUtxo)
    , payToScript (failProofValidator config) (InlineDatum failProofDatum) failProofValue
    , payToKey commitFoldOwner (assetClassValue adaAssetClass foldCollateral)
    , payToKey daoFeeKey (assetClassValue adaAssetClass (config.collateral - foldCollateral))
    , payToKey
        config.owner
        ( txBoxValue holderUtxo
            <> assetClassValue adaAssetClass (nodeAdaAmount + oilAdaAmount - config.collateral)
            <> burnedHolderToken
        )
    , mintValue (failProofMintingPolicy config) () failProofToken
    , memptyIf V.isZero burnedNodeToken (mintValue (nodeMintingPolicy config) ())
    , memptyIf V.isZero burnedHolderToken (mintValue (projectTokensHolderMintingPolicy config) ())
    , memptyIf V.isZero burnedCommitFoldToken (mintValue (commitFoldMintingPolicy config) ())
    , case action of
        NoBurningFold ->
          payToKey
            config.owner
            ( singleton
                (scriptCurrencySymbol (commitFoldMintingPolicy config))
                (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
                1
            )
        NoBurningNode ->
          payToKey
            config.owner
            ( singleton
                (scriptCurrencySymbol (nodeMintingPolicy config))
                (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
                1
            )
        _ -> mempty
    ]
  where
    commitFoldDatum = txBoxDatum commitFoldUtxo
    commitFoldOwner = addressToPubKeyHash commitFoldDatum.owner
    foldCollateral = commitFoldCollateral nodes
    daoFeeKey = addressToPubKeyHash config.daoFeeReceiver
    nodes = commitFoldDatum.nodeCount
    failProofDatum = toValidatorHash (nodeValidator config)
    failProofToken =
      singleton
        (scriptCurrencySymbol (failProofMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (failProofValidator config)))
        1
    failProofValue =
      failProofToken <> case action of
        MultipleTokens -> assetClassValue vUSDT 1
        _ -> mempty
    burnedNodeToken = case action of
      NoBurningNode -> mempty
      _ ->
        singleton
          (scriptCurrencySymbol (nodeMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (nodeValidator config)))
          (-1)
    burnedHolderToken = case action of
      NoBurningHolder -> mempty
      _ ->
        singleton
          (scriptCurrencySymbol (projectTokensHolderMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config)))
          (-1)
    burnedCommitFoldToken = case action of
      NoBurningFold -> mempty
      _ ->
        singleton
          (scriptCurrencySymbol (commitFoldMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
          (-1)
    commitFoldCollateral n = n * commitFoldFeeAdaAmount
