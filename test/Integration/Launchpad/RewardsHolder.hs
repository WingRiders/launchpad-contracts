module Integration.Launchpad.RewardsHolder where

import Data.Foldable (foldl')
import Data.Functor (void)
import Integration.Launchpad.Validators
import Integration.Mock (LaunchpadConfig (LaunchpadConfig, projectToken, raisingToken), rewardsHolderOilAdaAmount)
import Launchpad.Types (
  NodeKey,
  PoolProofDatum (PoolProofDatum),
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
  let value = rewardValue <> assetClassValue adaAssetClass rewardsHolderOilAdaAmount
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
  [(proofRef, _)] <- utxoAt (poolProofValidator config)

  tx <- case action of
    NoOwnerSignature -> signTx maliciousPkh $ spendRewardsHolderTx action config proofRef holderBoxes wallet
    _ -> signTx wallet $ spendRewardsHolderTx action config proofRef holderBoxes wallet
  void $ sendTx tx

spendRewardsHolderTx :: MaliciousRewardsHolderAction -> LaunchpadConfig -> TxOutRef -> [TxBox (TypedValidator RewardsHolderDatum ())] -> PubKeyHash -> Tx
spendRewardsHolderTx action config@LaunchpadConfig {projectToken, raisingToken} poolProofUtxo holderBoxes wallet =
  mconcat $
    map
      (spendBox (rewardsHolderValidator config) ())
      holderBoxes
      <> [ payToKey wallet rewardValue
         , case action of
            NoPoolProof -> mempty
            _ -> refInputHash poolProofUtxo poolProofDatum
         ]
  where
    poolProofDatum = PoolProofDatum projectCs projectTn raisingCs raisingTn
    AssetClass (projectCs, projectTn) = projectToken
    AssetClass (raisingCs, raisingTn) = raisingToken
    rewardValue = foldl' (\acc txBox -> (acc <> txBoxValue txBox)) mempty holderBoxes
