{-# LANGUAGE BlockArguments #-}

module Launchpad.Mint.ProjectTokensHolder where

import Launchpad.Types
import Plutarch
import Plutarch.Api.V1 (PCredential (PScriptCredential))
import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential))
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext (pfromPDatum, pisUTXOSpent, pscriptHashToTokenName, ptryFromInlineDatum, ptxSignedBy)
import Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )
import Plutarch.Lift
import Plutarch.Mint.Util (pvalueOfSingleton')
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Types.Base (PTimestamps (..))
import Plutarch.Util (
  pand'List,
  pcond,
  pcountOfUniqueTokens,
  perrorIfFalse,
  pfiniteTxValidityRangeTimestamps,
  pownCurrencySymbol,
  (#>=),
 )
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Project Tokens Holder Minting Policy

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data TokensHolderPolicyConfig = TokensHolderPolicyConfig
  { owner :: PubKeyHash
  , startTime :: POSIXTime
  , totalTokens :: Integer
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , collateral :: Integer
  , starter :: TxOutRef
  , nodeSymbol :: CurrencySymbol
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''TokensHolderPolicyConfig [('TokensHolderPolicyConfig, 0)]
PlutusTx.makeLift ''TokensHolderPolicyConfig

data PTokensHolderPolicyConfig (s :: S)
  = PTokensHolderPolicyConfig
      ( Term
          s
          ( PDataRecord
              [ "owner" ':= PPubKeyHash
              , "startTime" ':= PPOSIXTime
              , "totalTokens" ':= PInteger
              , "projectSymbol" ':= PCurrencySymbol
              , "projectToken" ':= PTokenName
              , "collateral" ':= PInteger
              , "starter" ':= PTxOutRef
              , "nodeSymbol" ':= PCurrencySymbol
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PTokensHolderPolicyConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTokensHolderPolicyConfig where
  type PLifted PTokensHolderPolicyConfig = TokensHolderPolicyConfig

deriving via
  (DerivePConstantViaData TokensHolderPolicyConfig PTokensHolderPolicyConfig)
  instance
    (PConstantDecl TokensHolderPolicyConfig)

pprojectTokensHolderMintingPolicy :: Term s (PTokensHolderPolicyConfig :--> PMintingPolicy)
pprojectTokensHolderMintingPolicy = phoistAcyclic $
  plam $ \cfg _redeemer context ->
    popaque $ pprojectTokensHolderMintingPolicyTyped # cfg # context

{- |
Initialize the launchpad:
  One launchpad holder token is minted
  One list element token is minted
  The utxo with the launchpad holder token is assumed to be a launchpad holder script,
    its script hash must be equal to the token name of the minted launchpad holder token
  At least collateral ADA must be locked into the launchpad holder utxo
  Launchpad holder utxo holds 3 unique tokens, ADA, launchpad holder token and project token
  The transaction is signed by the owner of the launchpad
  The starter utxo is spent
The following must be true of the configuration values:
  the upper current time approximation < start time
  the totalTokens field is equal to the number of the project tokens on the holder utxo
  the nodeScriptHash field must be equal to the token name of the minted list element token
Burn:
  One launchpad holder token is burned
-}
pprojectTokensHolderMintingPolicyTyped :: Term s (PTokensHolderPolicyConfig :--> PScriptContext :--> PUnit)
pprojectTokensHolderMintingPolicyTyped = phoistAcyclic $ plam \cfg context -> unTermCont do
  ctxF <- pletFieldsC @["txInfo", "purpose"] context
  infoF <- pletFieldsC @["inputs", "mint", "outputs", "signatories", "validRange"] ctxF.txInfo
  ownSymbol <- pletC (pownCurrencySymbol ctxF.purpose)
  mint <- pletC infoF.mint
  PPair ownTn ownCount <- pmatchC (pvalueOfSingleton' # ownSymbol # mint)
  cfgF <-
    pletFieldsC
      @'["owner", "startTime", "totalTokens", "projectSymbol", "projectToken", "collateral", "starter", "nodeSymbol"]
      cfg
  pure $
    ( pcond
        [
          ( ptraceIfFalse "E1" $ ownCount #== 1
          , perrorIfFalse #$ pmatch (pvalueOfSingleton' # cfgF.nodeSymbol # mint) \(PPair nodeTn nodeCount) ->
              pmatch (pfiniteTxValidityRangeTimestamps # infoF.validRange) \(PTimestamps _ upper) ->
                pand'List
                  [ ptraceIfFalse "E2" $ pisUTXOSpent # cfgF.starter # infoF.inputs
                  , ptraceIfFalse "E3" $ ptxSignedBy # infoF.signatories # cfgF.owner
                  , ptraceIfFalse "E4" $ upper #< cfgF.startTime
                  , ptraceIfFalse "E5" $
                      pany
                        # pisCorrectTokensHolder
                          (cfgF.totalTokens, cfgF.collateral)
                          (cfgF.projectSymbol, cfgF.projectToken)
                          (ownSymbol, ownTn)
                          nodeTn
                        # infoF.outputs
                  , ptraceIfFalse "E6" $ nodeCount #== 1
                  ]
          )
        , (ownCount #== -1, pconstant ())
        ]
        (ptraceError "E7")
    )
  where
    pisCorrectTokensHolder ::
      (Term s PInteger, Term s PInteger) ->
      (Term s PCurrencySymbol, Term s PTokenName) ->
      (Term s PCurrencySymbol, Term s PTokenName) ->
      Term s PTokenName ->
      Term s (PTxOut :--> PBool)
    pisCorrectTokensHolder (totalTokens, collateral) (projectSymbol, projectToken) (ownSymbol, mintedToken) nodeTn =
      plam \o ->
        pletFields @'["address", "value", "datum"] o \oF ->
          pmatch (pfield @"credential" # oF.address) \case
            PScriptCredential hash ->
              (ptraceIfFalse "E8" $ pscriptHashToTokenName (pfromData (pfield @"_0" # hash)) #== mintedToken)
                #&& pand'List
                  [ ptraceIfFalse "E9" $ pvalueOf # oF.value # ownSymbol # mintedToken #== 1
                  , ptraceIfFalse "E10" $ pcountOfUniqueTokens # oF.value #== 3
                  , ptraceIfFalse "E11" $ pvalueOf # oF.value # projectSymbol # projectToken #== totalTokens
                  , ptraceIfFalse "E12" $ pvalueOf # oF.value # padaSymbol # padaToken #>= collateral
                  , ptraceIfFalse "E13" $ pmatch
                      (pfromPDatum #$ ptryFromInlineDatum # oF.datum)
                      \(PLaunchpadTokensHolderDatum nodeScriptHash) ->
                        pscriptHashToTokenName (pfromData nodeScriptHash) #== nodeTn
                  ]
            PPubKeyCredential _ -> pconstant False

projectTokensHolderMintingPolicy :: TokensHolderPolicyConfig -> Script
projectTokensHolderMintingPolicy cfg = toScript $ pprojectTokensHolderMintingPolicy # pconstant cfg

projectTokensHolderMintingPolicySymbol :: TokensHolderPolicyConfig -> CurrencySymbol
projectTokensHolderMintingPolicySymbol cfg = CurrencySymbol $ getScriptHash $ scriptHash (projectTokensHolderMintingPolicy cfg)

projectTokensHolderPolicyScript :: Script
projectTokensHolderPolicyScript = toScript pprojectTokensHolderMintingPolicy

projectTokensHolderPolicySymbol :: CurrencySymbol
projectTokensHolderPolicySymbol = CurrencySymbol $ getScriptHash $ scriptHash projectTokensHolderPolicyScript
