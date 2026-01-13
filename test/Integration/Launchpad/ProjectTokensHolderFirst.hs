module Integration.Launchpad.ProjectTokensHolderFirst where

import Integration.Launchpad.Validators
import Integration.Mock
import Integration.Util
import Launchpad.Mint.ProjectTokensHolder qualified as PTH
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutus.Model
import Plutus.Util (adaAssetClass)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue)
import PlutusLedgerApi.V2 (PubKeyHash, Value, singleton)

createFirstProjectTokensHolder :: LaunchpadConfig -> PubKeyHash -> Integer -> AddToken -> Run ()
createFirstProjectTokensHolder config wallet distributedTokens addToken = do
  let AssetClass (projectSymbol, projectToken) = config.projectToken
      AssetClass (raisingSymbol, raisingToken) = config.raisingToken
      value =
        singleton projectSymbol projectToken config.totalTokens
          <> singleton raisingSymbol raisingToken distributedTokens
          <> assetClassValue adaAssetClass config.collateral
          <> case addToken of
            AddToken ->
              singleton
                (PTH.projectTokensHolderMintingPolicySymbol (tokensHolderPolicyConfig config))
                (scriptHashToTokenName (toValidatorHash (projectTokensHolderFirstValidator config)))
                1
            SkipToken -> mempty
  usp <- spend wallet value
  ensureTx wallet (createFirstProjectTokensHolderTx config usp value)

createFirstProjectTokensHolderTx :: LaunchpadConfig -> UserSpend -> Value -> Tx
createFirstProjectTokensHolderTx config usp value =
  mconcat
    [ userSpend usp
    , payToScript (projectTokensHolderFirstValidator config) (InlineDatum (toValidatorHash (nodeValidator config))) value
    ]

createFirstProjectTokensHolderRefScript :: LaunchpadConfig -> PubKeyHash -> Run ()
createFirstProjectTokensHolderRefScript config wallet = do
  usp <- spend wallet (adaValue 1)
  ensureTx wallet (loadRefScript (projectTokensHolderFirstValidator config) (adaValue 1) <> userSpend usp)
