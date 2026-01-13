{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.RewardsFold where

import Launchpad.Types
import Plutarch
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Mint.Util
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util
import PlutusLedgerApi.V2

{- | Parameters of the Rewards Fold Minting Policy (TxOutRef of the "starter" UTxO)

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
type RewardsFoldPolicyConfig = TxOutRef

newtype PRewardsFoldPolicyConfig (s :: S)
  = PRewardsFoldPolicyConfig (Term s (PAsData PTxOutRef))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRewardsFoldPolicyConfig where
  type DPTStrat _ = PlutusTypeNewtype

{- | Rewards fold minting policy.

Supports initializing the fold and unconditionally burning the token.

Initialize the fold:
* there is one rewards fold output utxo R
* 1 rewards fold token is minted, its token name is equal to the validator hash of the R utxo where it's placed
* there should be an input with the validator hash equal to the rewards fold nodeScriptHash field
* the redeemer of the nodeScriptHash input is the StartRewardsFold redeemer

Burn:
* one rewards fold validity token is burned
-}
prewardsFoldMintingPolicy :: Term s (PRewardsFoldPolicyConfig :--> PMintingPolicy)
prewardsFoldMintingPolicy = plam \cfg _redeemer context ->
  popaque (perrorIfFalse #$ ptraceIfFalse "F0" $ pvalidateRewardsFoldToken # cfg # context)

pvalidateRewardsFoldToken :: Term s (PRewardsFoldPolicyConfig :--> PScriptContext :--> PBool)
pvalidateRewardsFoldToken = plam \_cfg context -> unTermCont do
  contextFields <- pletFieldsC @'["purpose", "txInfo"] context
  tx <- pletFieldsC @'["inputs", "outputs", "mint", "redeemers"] contextFields.txInfo
  foldCs <- pletC (pownCurrencySymbol contextFields.purpose)
  PPair foldTn mintedAmount <- pmatchC (pvalueOfSingleton tx.mint foldCs)
  pure $
    pcond
      [ (mintedAmount #== 1, pvalidateRewardsTokenInitialization foldCs foldTn tx.inputs tx.outputs tx.redeemers)
      , (mintedAmount #== (-1), ptrue)
      ]
      (ptraceError "minted wrong rewards fold tokens amount")

pvalidateRewardsTokenInitialization ::
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PMap 'Unsorted PScriptPurpose PRedeemer) ->
  Term s PBool
pvalidateRewardsTokenInitialization foldCs foldTn inputs outputs redeemers =
  pmatch (passertSingleton "F1" # (pfindScriptOutputs # ptokenNameAsScriptHash foldTn # outputs)) \(PPair foldV foldD) -> unTermCont do
    node <- pletC $ pmatch (pfromPDatum @PRewardsFoldDatum # (ptryFromInlineDatum # foldD)) \case
      PRewardsFoldDatum fold -> pfield @"nodeScriptHash" # fold
    pure $
      pand'List
        [ ptraceIfFalse "F2" $
            pany
              # plam
                ( \i ->
                    ( ppaysToCredential
                        # node
                        # (ptxInInfoResolved # i)
                    )
                      #&& pisStartRewardsFoldConstructor (ptryTxOutRefRedeemer # (pfield @"outRef" # i) # redeemers)
                )
              # inputs
        , ptraceIfFalse "F3" $ pvalueOf # foldV # foldCs # foldTn #== 1
        ]

rewardsFoldMintingPolicy :: RewardsFoldPolicyConfig -> Script
rewardsFoldMintingPolicy cfg = toScript $ prewardsFoldMintingPolicy # pcon (PRewardsFoldPolicyConfig (pdata (pconstant cfg)))

rewardsFoldMintingPolicySymbol :: RewardsFoldPolicyConfig -> CurrencySymbol
rewardsFoldMintingPolicySymbol cfg = CurrencySymbol $ getScriptHash $ scriptHash (rewardsFoldMintingPolicy cfg)

rewardsFoldPolicyScript :: Script
rewardsFoldPolicyScript = toScript prewardsFoldMintingPolicy

rewardsFoldPolicySymbol :: CurrencySymbol
rewardsFoldPolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash rewardsFoldPolicyScript
