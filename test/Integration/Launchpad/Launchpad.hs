module Integration.Launchpad.Launchpad where

import Data.Functor (void)
import Integration.Launchpad.Node (findNodeUtxo)
import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util (actualTime)
import Launchpad.Types (
  LaunchpadTokensHolderDatum,
  LaunchpadTokensHolderFirstRedeemer (..),
  Node (..),
 )
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value (
  assetClassValue,
 )
import PlutusLedgerApi.V2
import PlutusTx.Prelude (inv)

data LaunchpadAction = NoPool | PoolExists | Fails

data MaliciousLaunchpadAction
  = None
  | AdditionalNodeTokenToUser
  | AdditionalHolderTokenToUser
  | CreateTimeNotCurrent
  | CancelTimeAfterStart
  | KeyJust
  | LessProjectTokensLocked
  | NextJust
  | NonZeroCommitted
  | NoOwnerSignature
  | NotEnoughOil
  | StartTimeInPast
  | WrongHolderTokenName
  | WrongNodeTokenName
  | TokenNotBurned

maliciousScriptHash :: ScriptHash
maliciousScriptHash = "36bea2acff0a1c9376b0fd4137ee46fb0f7acfd173ec071e338f8001"

maliciousPkh :: PubKeyHash
maliciousPkh = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

createLaunchpad :: MaliciousLaunchpadAction -> LaunchpadConfig -> PubKeyHash -> Run ()
createLaunchpad action config@LaunchpadConfig {projectToken, totalTokens} wallet = do
  now <- actualTime
  let nodeDatum =
        Node
          { key = case action of
              KeyJust -> Just ("aaaa", 0)
              _ -> Nothing
          , next = case action of
              NextJust -> Just ("aaaa", 0)
              _ -> Nothing
          , createdTime = case action of
              CreateTimeNotCurrent -> now - 1
              _ -> now + 1_000
          , committed = case action of
              NonZeroCommitted -> 1
              _ -> 0
          }
  let holderDatum = toValidatorHash (nodeValidator config)

  usp <-
    spend
      wallet
      ( assetClassValue projectToken totalTokens
          <> assetClassValue adaAssetClass (nodeAdaAmount + config.collateral)
      )
  tx <- case action of
    NoOwnerSignature -> signTx maliciousPkh $ createLaunchpadTx action config usp nodeDatum holderDatum
    _ -> signTx wallet $ createLaunchpadTx action config usp nodeDatum holderDatum
  void $ sendTx =<< validateIn (interval now (now + 1_000)) tx

createLaunchpadTx :: MaliciousLaunchpadAction -> LaunchpadConfig -> UserSpend -> Node -> LaunchpadTokensHolderDatum -> Tx
createLaunchpadTx action config@LaunchpadConfig {projectToken, totalTokens} usp nodeDatum holderDatum =
  mconcat
    [ mintValue (nodeMintingPolicy config) () nodeMint
    , mintValue (projectTokensHolderMintingPolicy config) () holderMint
    , payToScript (nodeValidator config) (InlineDatum nodeDatum) nodeValue
    , payToScript (projectTokensHolderFirstValidator config) (InlineDatum holderDatum) holderValue
    , userSpend usp
    , case action of
        AdditionalNodeTokenToUser -> payToKey maliciousPkh (singleton (scriptCurrencySymbol (nodeMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (nodeValidator config))) 1)
        AdditionalHolderTokenToUser -> payToKey maliciousPkh (singleton (scriptCurrencySymbol (projectTokensHolderMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config))) 1)
        LessProjectTokensLocked -> payToKey maliciousPkh (assetClassValue projectToken 1)
        _ -> mempty
    ]
  where
    nodeMint = case action of
      WrongNodeTokenName -> singleton (scriptCurrencySymbol (nodeMintingPolicy config)) "NotScriptHash" 1
      AdditionalNodeTokenToUser -> singleton (scriptCurrencySymbol (nodeMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (nodeValidator config))) 2
      _ -> singleton (scriptCurrencySymbol (nodeMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (nodeValidator config))) 1
    holderMint = case action of
      WrongHolderTokenName -> singleton (scriptCurrencySymbol (projectTokensHolderMintingPolicy config)) "NotScriptHash" 1
      AdditionalHolderTokenToUser -> singleton (scriptCurrencySymbol (projectTokensHolderMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config))) 2
      _ -> singleton (scriptCurrencySymbol (projectTokensHolderMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config))) 1
    nodeValue =
      nodeMint <> assetClassValue adaAssetClass nodeAdaAmount <> case action of
        AdditionalNodeTokenToUser -> inv (singleton (scriptCurrencySymbol (nodeMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (nodeValidator config))) 1)
        NotEnoughOil -> inv (assetClassValue adaAssetClass 1)
        _ -> mempty
    holderValue =
      holderMint <> assetClassValue projectToken totalTokens <> assetClassValue adaAssetClass config.collateral <> case action of
        AdditionalHolderTokenToUser -> inv (singleton (scriptCurrencySymbol (projectTokensHolderMintingPolicy config)) (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config))) 1)
        LessProjectTokensLocked -> inv (assetClassValue projectToken 1)
        _ -> mempty

cancelLaunchpad :: MaliciousLaunchpadAction -> LaunchpadConfig -> Bool -> PubKeyHash -> Run ()
cancelLaunchpad action config refHeadNode wallet = do
  now <- actualTime

  [holderUtxo] <- boxAt (projectTokensHolderFirstValidator config)
  nodes <- utxoAt (nodeValidator config)
  let nodeUtxo = case refHeadNode of
        True -> Just (findNodeUtxo Nothing nodes)
        False -> Nothing

  tx <- case action of
    NoOwnerSignature -> signTx maliciousPkh $ cancelLaunchpadTx action config nodeUtxo holderUtxo
    _ -> signTx wallet $ cancelLaunchpadTx action config nodeUtxo holderUtxo

  void $ sendTx =<< validateIn (interval now (now + 1000)) tx

cancelLaunchpadTx ::
  MaliciousLaunchpadAction ->
  LaunchpadConfig ->
  Maybe (TxOutRef, TxOut) ->
  TxBox (TypedValidator LaunchpadTokensHolderDatum LaunchpadTokensHolderFirstRedeemer) ->
  Tx
cancelLaunchpadTx action config nodeRef holder =
  mconcat
    [ spendBox (projectTokensHolderFirstValidator config) CancelLaunchpad holder
    , payToKey config.owner walletValue
    , case action of
        TokenNotBurned -> mempty
        _ -> mintValue (projectTokensHolderMintingPolicy config) () burnedHolderToken
    , maybe mempty (refInputInline . fst) nodeRef
    ]
  where
    walletValue =
      txBoxValue holder <> case action of
        TokenNotBurned -> mempty
        _ -> burnedHolderToken
    burnedHolderToken =
      singleton
        (scriptCurrencySymbol (projectTokensHolderMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config)))
        (-1)
