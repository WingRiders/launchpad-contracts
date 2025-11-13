{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Integration.Launchpad.CommitFold where

import Data.List (sortBy)
import Data.Maybe
import Integration.Launchpad.Node hiding (None)
import Integration.Launchpad.Validators
import Integration.Mock (LaunchpadConfig (..), oilAdaAmount)
import Integration.Util
import Launchpad.Node qualified as N
import Launchpad.Types
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutus.Model
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2

data MaliciousCommitFoldAction
  = None
  | DoubleSatisfy

initCommitFoldMalicious :: LaunchpadConfig -> Address -> PubKeyHash -> Maybe CommitFoldDatum -> Maybe NodeKey -> Run ()
initCommitFoldMalicious config owner wallet foldDatum nodeKey = do
  nodeUtxos <- utxoAt (nodeValidator config)
  let headNodeUtxo = findNodeUtxo nodeKey nodeUtxos
  usp <- spend wallet (adaValue oilAdaAmount)
  range <- currentTimeRad 1_000
  submitTx wallet =<< validateIn range (initCommitFoldTx config usp owner headNodeUtxo foldDatum)

initCommitFold :: LaunchpadConfig -> Address -> PubKeyHash -> Maybe CommitFoldDatum -> Maybe NodeKey -> Run ()
initCommitFold = initCommitFoldMalicious

createCommitFold :: LaunchpadConfig -> PubKeyHash -> CommitFoldDatum -> AddToken -> Run ()
createCommitFold config wallet datum addToken = do
  let value =
        adaValue oilAdaAmount
          <> case addToken of
            AddToken ->
              singleton
                (scriptCurrencySymbol (commitFoldMintingPolicy config))
                (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
                1
            SkipToken -> mempty

  usp <- spend wallet value
  ensureTx wallet (createCommitFoldTx config usp datum value)

{- | Commit fold application includes the following:
 - spending the commit fold utxo
 - referencing some node utxos in the correct order
 - recreating the commit fold utxo with the updated committed field and the next field
-}
commitFoldOver :: LaunchpadConfig -> PubKeyHash -> Maybe CommitFoldDatum -> [Maybe NodeKey] -> MaliciousCommitFoldAction -> Run ()
commitFoldOver config wallet resultingDatum nodeKeys action = do
  nodes <- filter (isNothing . txOutReferenceScript . snd) <$> utxoAt (nodeValidator config)
  let keyedNodes =
        filter
          ( \(_, nodeKey) ->
              elem (key . txOutputDatum $ nodeKey) nodeKeys
          )
          nodes
      sortedNodes =
        sortBy
          ( \((_, a), _) ((_, b), _) ->
              compare (key . txOutputDatum $ a) (key . txOutputDatum $ b)
          )
          (zip keyedNodes [0 ..])
  commitFoldUtxos <- filter (isNothing . txOutReferenceScript . snd) <$> utxoAt (commitFoldValidator config)
  submitTx wallet (commitFoldOverTx config wallet commitFoldUtxos resultingDatum sortedNodes action)

commitFoldOverTx :: LaunchpadConfig -> PubKeyHash -> [(TxOutRef, TxOut)] -> Maybe CommitFoldDatum -> [((TxOutRef, TxOut), Integer)] -> MaliciousCommitFoldAction -> Tx
commitFoldOverTx config wallet commitFoldUtxos resultingDatum nodeUtxos action =
  case action of
    None ->
      mconcat $
        map (refInputInline . fst . fst) nodeUtxos
          <> [ spendScript
                (commitFoldValidator config)
                (fst commitFoldUtxo)
                (CommitFold (map snd nodeUtxos))
                oldDatum
             , payToScript
                (commitFoldValidator config)
                (InlineDatum newDatum)
                (txOutValue (snd commitFoldUtxo))
             ]
    DoubleSatisfy ->
      mconcat $
        map (refInputInline . fst . fst) nodeUtxos
          <> [ spendScript
                (commitFoldValidator config)
                (fst commitFoldUtxo)
                (CommitFold (map snd nodeUtxos))
                oldDatum
             , spendScript
                (commitFoldValidator config)
                (fst commitFoldUtxo2)
                (CommitFold (map snd nodeUtxos))
                oldDatum
             , payToScript
                (commitFoldValidator config)
                (InlineDatum newDatum)
                (txOutValue (snd commitFoldUtxo))
             , payToKey wallet stealValue
             ]
  where
    stealValue =
      adaValue oilAdaAmount
        <> singleton
          (scriptCurrencySymbol (commitFoldMintingPolicy config))
          (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
          1

    commitFoldUtxo = head commitFoldUtxos
    commitFoldUtxo2 = commitFoldUtxos !! 1

    oldDatum = txOutputDatum (snd commitFoldUtxo)
    lastNodeDatum = txOutputDatum @Node (snd (fst (last nodeUtxos)))
    newDatum = case resultingDatum of
      Just d -> d
      Nothing ->
        ( (oldDatum :: CommitFoldDatum)
            { committed = countCommitted oldDatum.cutoffTime oldDatum.cutoffKey oldDatum.overcommitted (map (txOutputDatum . snd . fst) nodeUtxos)
            , nodeCount = oldDatum.nodeCount + toInteger (length nodeUtxos)
            , next = lastNodeDatum.next
            }
        )

nodeCommitment :: Maybe POSIXTime -> Maybe NodeKey -> Integer -> Node -> Integer
nodeCommitment (Just cutoffTime) (Just cutoffKey) overcommitted node@(Node (Just nodeKey) _ _ _)
  | node.createdTime < cutoffTime = node.committed
  | node.createdTime > cutoffTime = 0
  | nodeKey < cutoffKey = node.committed
  | nodeKey > cutoffKey = 0
  | otherwise = node.committed - overcommitted
nodeCommitment _ _ _ node = node.committed

countCommitted :: Maybe POSIXTime -> Maybe NodeKey -> Integer -> [Node] -> Integer
countCommitted cutoffTime cutoffKey overcommitted nodes = sum (map (nodeCommitment cutoffTime cutoffKey overcommitted) nodes)

createCommitFoldTx :: LaunchpadConfig -> UserSpend -> CommitFoldDatum -> Value -> Tx
createCommitFoldTx config usp datum value =
  mconcat
    [ userSpend usp
    , payToScript
        (commitFoldValidator config)
        (InlineDatum datum)
        value
    ]

initCommitFoldTx :: LaunchpadConfig -> UserSpend -> Address -> (TxOutRef, TxOut) -> Maybe CommitFoldDatum -> Tx
initCommitFoldTx config usp owner (nodeOutRef, nodeOut) datum =
  mconcat
    [ userSpend usp
    , mintValue (commitFoldMintingPolicy config) () mintedValue
    , refInputHash nodeOutRef (txOutDatum nodeOut)
    , payToScript (commitFoldValidator config) (InlineDatum commitFoldDatum) (mintedValue <> adaValue oilAdaAmount)
    ]
  where
    commitFoldDatum = case datum of
      Just d -> d
      Nothing ->
        CommitFoldDatum
          { nodeScriptHash = N.nodeScriptValidatorHash (nodeConfig config)
          , next = node.next
          , committed = 0
          , overcommitted = 0
          , cutoffKey = Nothing
          , cutoffTime = Nothing
          , nodeCount = 1
          , owner
          }

    node = case txOutDatum nodeOut of
      OutputDatum (Datum d) -> fromJust (fromBuiltinData @Node d)
      _ -> error "createCommitFoldTx: no datum"
    mintedValue =
      singleton
        (scriptCurrencySymbol (commitFoldMintingPolicy config))
        (scriptHashToTokenName (toValidatorHash (commitFoldValidator config)))
        1

createCommitFoldRefScript :: LaunchpadConfig -> PubKeyHash -> Run ()
createCommitFoldRefScript config wallet = do
  usp <- spend wallet (adaValue 1)
  ensureTx wallet (loadRefScript (commitFoldValidator config) (adaValue 1) <> userSpend usp)
