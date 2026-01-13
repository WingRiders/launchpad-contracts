module Integration.Launchpad.RewardsHolder where

import Data.Foldable (foldl')
import Data.Functor (void)
import Integration.Launchpad.Validators
import Integration.Mock (LaunchpadConfig (LaunchpadConfig, projectToken, raisingToken), oilAdaAmount)
import Launchpad.Types (
  NodeKey,
  RewardsHolderDatum (..),
 )
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2

data MaliciousRewardsHolderAction
  = None
  | NoOwnerSignature
  | NoPoolProof
  | WrongPoolProof

maliciousPkh :: PubKeyHash
maliciousPkh = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

maliciousCs :: CurrencySymbol
maliciousCs = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

findHolderBoxes :: PubKeyHash -> [TxBox (TypedValidator RewardsHolderDatum ())] -> [TxBox (TypedValidator RewardsHolderDatum ())]
findHolderBoxes wallet holders = case filter f holders of
  [] -> error "findHolderBoxes: no holder found"
  a -> a
  where
    f txBox = PubKeyHash (fst ((txBoxDatum txBox).owner)) == wallet

createRewardsHolder :: MaliciousRewardsHolderAction -> LaunchpadConfig -> Value -> NodeKey -> PubKeyHash -> Run ()
createRewardsHolder action config@LaunchpadConfig {projectToken, raisingToken} rewardValue rewardOwnerKey wallet = do
  let datum =
        RewardsHolderDatum
          { owner = rewardOwnerKey
          , projectSymbol = case action of
              WrongPoolProof -> maliciousCs
              _ -> projectCs
          , projectToken = projectTn
          , raisingSymbol = raisingCs
          , raisingToken = raisingTn
          }
  let value = rewardValue <> assetClassValue adaAssetClass oilAdaAmount
  usp <- spend wallet value
  tx <- signTx wallet $ createRewardsHolderTx config datum usp value
  void $ sendTx tx
  where
    AssetClass (projectCs, projectTn) = projectToken
    AssetClass (raisingCs, raisingTn) = raisingToken

createRewardsHolderTx :: LaunchpadConfig -> RewardsHolderDatum -> UserSpend -> Value -> Tx
createRewardsHolderTx config rewardsHolderDatum usp utxoValue =
  mconcat
    [ payToScript (rewardsHolderValidator config) (InlineDatum rewardsHolderDatum) utxoValue
    , userSpend usp
    ]

spendRewardsHolder :: MaliciousRewardsHolderAction -> LaunchpadConfig -> PubKeyHash -> Run ()
spendRewardsHolder action config wallet = do
  rewardsHolderUtxos <- boxAt (rewardsHolderValidator config)
  let holderBoxes = findHolderBoxes wallet rewardsHolderUtxos

  poolProofs <- utxoAt (poolProofValidator config)

  range <- currentTimeRad 1_000
  tx <- case action of
    NoOwnerSignature -> signTx maliciousPkh =<< validateIn range (spendRewardsHolderTx action config poolProofs holderBoxes wallet)
    _ -> signTx wallet =<< validateIn range (spendRewardsHolderTx action config poolProofs holderBoxes wallet)
  void $ sendTx tx

spendRewardsHolderTx ::
  MaliciousRewardsHolderAction ->
  LaunchpadConfig ->
  [(TxOutRef, TxOut)] ->
  [TxBox (TypedValidator RewardsHolderDatum ())] ->
  PubKeyHash ->
  Tx
spendRewardsHolderTx action config poolProofs holderBoxes wallet =
  mconcat $
    map
      (spendBox (rewardsHolderValidator config) ())
      holderBoxes
      <> [ payToKey wallet rewardValue
         , case action of
            NoPoolProof -> mempty
            _ -> mconcat . map (\(ref, out) -> refInputHash ref (txOutDatum out)) $ poolProofs
         ]
  where
    rewardValue = foldl' (\acc txBox -> (acc <> txBoxValue txBox)) mempty holderBoxes
