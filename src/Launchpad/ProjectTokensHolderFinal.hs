{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Launchpad.ProjectTokensHolderFinal (
  projectTokensHolderScriptValidator,
  projectTokensHolderScriptValidatorHash,
  projectTokensHolderScriptAddress,
  projectTokensHolderScript,
  projectTokensHolderValidator,
  TokenHolderRedeemerFinal (..),
  TokensHolderFinalConfig (..),
)
where

import Data.ByteString.Hash (blake2b_256)
import Data.Text.Encoding (encodeUtf8)
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Launchpad.Types (PPoolProofDatum)
import Launchpad.Util (pisCorrectPool)
import Other.Vesting (PVestingDatum (..))
import Plutarch
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromDatumHash, ptryFromInlineDatum)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util (
  pand'List,
  passertSingleton,
  pcond,
  pcountAllScriptInputs,
  pcountOfUniqueTokens,
  pcountOfUniqueTokensWithOverlap,
  pcountScriptInputs,
  pdnothing,
  perrorIfFalse,
  pgetInput,
  pisAda,
  ppaysToCredential,
  ptryFindOutputWithAsset,
  ptryLookup,
  ptxInInfoResolved,
  (#>),
  (#>=),
 )
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Project Tokens Holder Final Validator

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data TokensHolderFinalConfig = TokensHolderFinalConfig
  { owner :: Address
  , daoFeeUnits :: Integer
  , daoFeeBase :: Integer
  , tokensToDistribute :: Integer
  , raisedTokensPoolPartPercentage :: Integer
  , totalTokens :: Integer
  , collateral :: Integer
  , poolSymbol :: CurrencySymbol
  , poolValidatorHash :: ScriptHash
  , factoryValidatorHash :: ScriptHash
  , poolProofValidatorHash :: ScriptHash
  , vestingValidatorHash :: ScriptHash
  , vestingPeriodDuration :: POSIXTime
  , vestingPeriodDurationToFirstUnlock :: POSIXTime
  , vestingPeriodInstallments :: Integer
  , vestingPeriodStart :: POSIXTime
  , daoFeeReceiver :: Address
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , starter :: TxOutRef
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''TokensHolderFinalConfig [('TokensHolderFinalConfig, 0)]
PlutusTx.makeLift ''TokensHolderFinalConfig

data PTokensHolderFinalConfig (s :: S)
  = PTokensHolderFinalConfig
      ( Term
          s
          ( PDataRecord
              [ "owner" ':= PAddress
              , "daoFeeUnits" ':= PInteger
              , "daoFeeBase" ':= PInteger
              , "tokensToDistribute" ':= PInteger
              , "raisedTokensPoolPartPercentage" ':= PInteger
              , "totalTokens" ':= PInteger
              , "collateral" ':= PInteger
              , "poolSymbol" ':= PCurrencySymbol
              , "poolValidatorHash" ':= PScriptHash
              , "factoryValidatorHash" ':= PScriptHash
              , "poolProofValidatorHash" ':= PScriptHash
              , "vestingValidatorHash" ':= PScriptHash
              , "vestingPeriodDuration" ':= PPOSIXTime
              , "vestingPeriodDurationToFirstUnlock" ':= PPOSIXTime
              , "vestingPeriodInstallments" ':= PInteger
              , "vestingPeriodStart" ':= PPOSIXTime
              , "daoFeeReceiver" ':= PAddress
              , "raisingSymbol" ':= PCurrencySymbol
              , "raisingToken" ':= PTokenName
              , "projectSymbol" ':= PCurrencySymbol
              , "projectToken" ':= PTokenName
              , "starter" ':= PTxOutRef
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PTokensHolderFinalConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTokensHolderFinalConfig where
  type PLifted PTokensHolderFinalConfig = TokensHolderFinalConfig

deriving via
  (DerivePConstantViaData TokensHolderFinalConfig PTokensHolderFinalConfig)
  instance
    (PConstantDecl TokensHolderFinalConfig)

{- | Share token names are unique per pool.
In V2 they are computed in the following way:
shareTokenName = blake2b (blake2b poolType <> blake2b aScale <> blake2b bScale <> blake2b assetA <> blake2b assetB)

Note that we don't support stableswap pools in the launchpad,
hence the pool type and the scales are statically known
-}
pshareTokenName ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PTokenName
    )
pshareTokenName = plam \raisingSymbol raisingToken projectSymbol projectToken ->
  let assetRaisingHash = pblake2b_256 # (pto raisingSymbol <> pto raisingToken)
      assetProjectHash = pblake2b_256 # (pto projectSymbol <> pto projectToken)
      assetsHash =
        pcond
          [ (raisingSymbol #< projectSymbol, assetRaisingHash <> assetProjectHash)
          , (raisingSymbol #> projectSymbol, assetProjectHash <> assetRaisingHash)
          , (raisingToken #< projectToken, assetRaisingHash <> assetProjectHash)
          ]
          (assetProjectHash <> assetRaisingHash)
   in pcon . PTokenName $ pblake2b_256 # (pconstant staticPart <> assetsHash)
  where
    -- NOTE: these are computed offchain
    poolTypeIdHash = blake2b_256 "0"
    aScaleHash = blake2b_256 $ encodeUtf8 "1"
    bScaleHash = blake2b_256 $ encodeUtf8 "1"
    staticPart = poolTypeIdHash <> aScaleHash <> bScaleHash

ppaysAtleastToAddress :: Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PAddress :--> PTxOut :--> PBool)
ppaysAtleastToAddress = phoistAcyclic $
  plam \symbol token amount address out -> pletFields @["address", "value"] out \outF ->
    (outF.address #== address)
      #&& pand'List
        [ pvalueOf # outF.value # symbol # token #>= amount
        , pcountOfUniqueTokensWithOverlap symbol outF.value #== 2
        ]

data TokenHolderRedeemerFinal
  = PoolExists
  | NoPool
  deriving stock (Generic, Enum, Bounded)
  deriving (PlutusTx.ToData, PlutusTx.FromData) via (EnumIsData TokenHolderRedeemerFinal)

data PTokenHolderRedeemerFinal (s :: S)
  = PPoolExists
  | PNoPool
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PTokenHolderRedeemerFinal where
  type DPTStrat _ = PlutusTypeEnumData

instance PTryFrom PData (PAsData PTokenHolderRedeemerFinal)

{-
    The pool doesn't exist:
        there is a compensation output sent to the launchpad owner
        there is a pool output utxo with the correct pool validity token
        the pool output datum contains the correct asset pair
        there is a vesting contract output utxo
        the vesting contract output has all the shares given to the user by the pool
        the vesting contract output holds at most 2 unique tokens, ADA and shares
        a Dao fee is sent to the DAO wallet:
            all the remaining raised tokens times raisedTokensPoolPartPercentage divided by 100 are locked in the pool
        DAO wallet utxo holds at most 2 unique tokens, ADA and raised token
        all the project tokens from the launchpad project tokens holder utxo are locked in the pool
        the beneficiary of the vesting contract is the launchpad owner
        the vestingAsset of the vesting contract is the pool shares
        the totalVestingQty of the vesting contract is the total amount of shares given to the user
        the vestingPeriodStart of the vesting contract is equal to the value in the parameter
        the vestingPeriodEnd of the vesting contract is:
            the vestingPeriodStart + a predetermined value
        the firstUnlockPossibleAfter of the vesting contract is:
            the vestingPeriodStart + a predetermined value
        the totalInstallments of the vesting contract is a predetermined value
        there are two script inputs
        there is a factory script in the inputs
    The pool exists:
        there is a compensation output sent to the launchpad owner
        there is a pool proof utxo ref input
        there is a utxo output sent to the DAO wallet's address, its value is:
            the receiving tokens of the launchpad project tokens holder times raisedTokensPoolPartPercentage
            divided by 100 + the Dao fee + all of its project tokens
        DAO wallet utxo holds at most 3 unique tokens, ADA project token, raised token
        there are two script inputs
-}
projectTokensHolderValidatorTyped ::
  Term s (PTokensHolderFinalConfig :--> PTokenHolderRedeemerFinal :--> PScriptContext :--> PUnit)
projectTokensHolderValidatorTyped = phoistAcyclic $ plam \cfg redeemer context -> unTermCont do
  ctxF <- pletAllC context
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "mint", "datums", "referenceInputs"] ctxF.txInfo
  mint <- pletC infoF.mint
  txOutputs <- pletC infoF.outputs
  txInputs <- pletC infoF.inputs
  txRefInputs <- pletC infoF.referenceInputs
  PSpending ownRef <- pmatchC ctxF.purpose
  let ownInput = pgetInput # txInputs # (pfield @"_0" # ownRef)
  ownInputF <- pletFieldsC @["address", "value"] ownInput
  ownInputValue <- pletC ownInputF.value
  cfgF <-
    pletFieldsC
      @'[ "owner"
        , "daoFeeUnits"
        , "daoFeeBase"
        , "tokensToDistribute"
        , "raisedTokensPoolPartPercentage"
        , "totalTokens"
        , "collateral"
        , "poolSymbol"
        , "poolValidatorHash"
        , "factoryValidatorHash"
        , "vestingValidatorHash"
        , "vestingPeriodDuration"
        , "vestingPeriodDurationToFirstUnlock"
        , "vestingPeriodStart"
        , "vestingPeriodInstallments"
        , "daoFeeReceiver"
        , "poolProofValidatorHash"
        , "raisingSymbol"
        , "raisingToken"
        , "projectSymbol"
        , "projectToken"
        ]
      cfg
  raisingCs <- pletC cfgF.raisingSymbol
  raisingTn <- pletC cfgF.raisingToken
  projectCs <- pletC cfgF.projectSymbol
  projectTn <- pletC cfgF.projectToken
  poolCs <- pletC cfgF.poolSymbol
  plpShareTn <- pletC $ pshareTokenName # raisingCs # raisingTn # projectCs # projectTn

  numRaised <-
    pletC $
      pif
        (pisAda # raisingCs)
        (pvalueOf # ownInputValue # raisingCs # raisingTn - cfgF.collateral)
        (pvalueOf # ownInputValue # raisingCs # raisingTn)

  pure
    $ perrorIfFalse
      #$ ptraceIfFalse "J0"
    $ pand'List
      [ pmatch redeemer \case
          PNoPool ->
            pvalidateNoPool
              cfgF.owner
              cfgF.factoryValidatorHash
              (cfgF.daoFeeReceiver, cfgF.daoFeeUnits, cfgF.daoFeeBase)
              (cfgF.vestingValidatorHash, cfgF.vestingPeriodInstallments, cfgF.vestingPeriodDuration, cfgF.vestingPeriodDurationToFirstUnlock, cfgF.vestingPeriodStart)
              (poolCs, cfgF.poolValidatorHash, plpShareTn)
              (projectCs, projectTn)
              (raisingCs, raisingTn)
              (numRaised, cfgF.raisedTokensPoolPartPercentage, cfgF.collateral)
              ownInputValue
              txInputs
              txOutputs
              infoF.datums
              mint
          PPoolExists ->
            pvalidatePoolExists
              cfgF.owner
              (cfgF.daoFeeReceiver, cfgF.daoFeeUnits, cfgF.daoFeeBase)
              cfgF.poolProofValidatorHash
              (projectCs, projectTn)
              (raisingCs, raisingTn)
              (numRaised, cfgF.raisedTokensPoolPartPercentage, cfgF.collateral)
              ownInputValue
              txInputs
              txOutputs
              txRefInputs
      ]

pvalidatePoolExists ::
  Term s PAddress ->
  (Term s PAddress, Term s PInteger, Term s PInteger) ->
  Term s PScriptHash ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PInteger, Term s PInteger, Term s PInteger) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s PBool
pvalidatePoolExists
  owner
  (daoFeeReceiver, daoFeeUnits, daoFeeBase)
  poolProofValidatorHash
  (projectCs, projectTn)
  (raisingCs, raisingTn)
  (numRaised, raisedTokensPoolPartPercentage, returnedCollateral)
  ownInputValue
  txInputs
  txOutputs
  txRefInputs = unTermCont do
    let poolProofTxIn =
          passertSingleton "one pool proof"
            #$ pfilter
            # plam
              (\o -> (ppaysToCredential # poolProofValidatorHash # (ptxInInfoResolved # o)))
            # txRefInputs
        poolProofOut = pfromData (ptxInInfoResolved # poolProofTxIn)
        poolProofDatum = pfromPDatum @PPoolProofDatum # (ptryFromInlineDatum #$ pfield @"datum" # poolProofOut)
    poolProofFields <- pletFieldsC @'["projectSymbol", "projectToken", "raisingSymbol", "raisingToken"] poolProofDatum
    owedToDao <- pletC (pdiv # (daoFeeUnits * numRaised) # daoFeeBase)
    projectTokens <- pletC (pvalueOf # ownInputValue # projectCs # projectTn)
    raisedToDao <- pletC ((pdiv # ((numRaised - owedToDao) * raisedTokensPoolPartPercentage) # 100) + owedToDao)
    let owedToOwner = numRaised - raisedToDao
    pure $
      pand'List
        [ -- The project tokens holder
          pcountAllScriptInputs # txInputs #== 1
        , poolProofFields.projectSymbol #== projectCs
        , poolProofFields.projectToken #== projectTn
        , poolProofFields.raisingSymbol #== raisingCs
        , poolProofFields.raisingToken #== raisingTn
        , pany
            # plam
              ( \txOut -> unTermCont do
                  txoFields <- pletFieldsC @["address", "value"] txOut
                  txOutValue <- pletC txoFields.value
                  pure $
                    (txoFields.address #== daoFeeReceiver)
                      #&& pand'List
                        [ pvalueOf # txOutValue # raisingCs # raisingTn #== raisedToDao
                        , pvalueOf # txOutValue # projectCs # projectTn #== projectTokens
                        , pcountOfUniqueTokensWithOverlap raisingCs txOutValue #== 3
                        ]
              )
            # txOutputs
        , pany
            # plam
              ( \o -> unTermCont do
                  txoFields <- pletFieldsC @["address", "value"] o
                  pure $
                    (txoFields.address #== owner)
                      #&& pand'List
                        [ pif
                            (pisAda # raisingCs)
                            (pvalueOf # txoFields.value # raisingCs # raisingTn #>= returnedCollateral + owedToOwner)
                            ( pand'List
                                [ pvalueOf # txoFields.value # raisingCs # raisingTn #== owedToOwner
                                , pvalueOf # txoFields.value # padaSymbol # padaToken #>= returnedCollateral
                                ]
                            )
                        , -- ada && raising token
                          pcountOfUniqueTokensWithOverlap raisingCs txoFields.value #== 2
                        ]
              )
            # txOutputs
        ]

pvalidateNoPool ::
  Term s PAddress ->
  Term s PScriptHash ->
  (Term s PAddress, Term s PInteger, Term s PInteger) ->
  (Term s PScriptHash, Term s PInteger, Term s PPOSIXTime, Term s PPOSIXTime, Term s PPOSIXTime) ->
  (Term s PCurrencySymbol, Term s PScriptHash, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PInteger, Term s PInteger, Term s PInteger) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PMap 'Unsorted PDatumHash PDatum) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PBool
pvalidateNoPool
  owner
  factoryValidatorHash
  (daoFeeReceiver, daoFeeUnits, daoFeeBase)
  (vestingValidatorHash, vestingPeriodInstallments, vestingPeriodDuration, vestingPeriodDurationToFirstUnlock, vestingPeriodStartTime)
  (poolCs, poolValidatorHash, plpShareTn)
  (projectCs, projectTn)
  (raisingCs, raisingTn)
  (numRaised, raisedTokensPoolPartPercentage, returnedCollateral)
  ownInputValue
  txInputs
  txOutputs
  datums
  mint = unTermCont do
    owedToDao <- pletC $ pdiv # (daoFeeUnits * numRaised) # daoFeeBase
    owedToPool <- pletC $ pdiv # ((numRaised - owedToDao) * raisedTokensPoolPartPercentage) # 100
    owedToOwner <- pletC $ numRaised - owedToDao - owedToPool

    let mintedShares = pvalueOf # mint # poolCs # plpShareTn
        poolUTxO =
          ptryFindOutputWithAsset
            # poolValidatorHash
            # poolCs
            # pconstant C.lpValidityTokenName
            # 1
            # txOutputs

    poolOutputF <- pletFieldsC @["address", "datum", "value"] poolUTxO

    let poolDatum = pfromPDatum @PWrPoolConstantProductDatum #$ (ptryFromInlineDatum # poolOutputF.datum)

    poolDatumF <- pletFieldsC @'["assetASymbol", "assetAToken", "assetBSymbol", "assetBToken"] poolDatum

    let correctPool =
          pisCorrectPool
            (projectCs, projectTn)
            (raisingCs, raisingTn)
            (poolDatumF.assetASymbol, poolDatumF.assetAToken)
            (poolDatumF.assetBSymbol, poolDatumF.assetBToken)

    let poolShareTokens =
          pvalueOf
            # poolOutputF.value
            # poolCs
            # plpShareTn

    userGivenShares <- pletC (mintedShares - poolShareTokens)

    let vestingOutput =
          ptryFindOutputWithAsset
            # vestingValidatorHash
            # poolCs
            # plpShareTn
            # userGivenShares
            # txOutputs
    vesting <- pletFieldsC @["datum", "value"] vestingOutput
    let vestingDatum = pfromPDatum @PVestingDatum #$ ptryLookup # (ptryFromDatumHash # vesting.datum) # datums

    vestingDatumF <-
      pletFieldsC
        @[ "beneficiary"
         , "vestingSymbol"
         , "vestingToken"
         , "totalVestingQty"
         , "vestingPeriodStart"
         , "vestingPeriodEnd"
         , "firstUnlockPossibleAfter"
         , "totalInstallments"
         ]
        vestingDatum
    pure $
      pand'List
        [ -- The factory and the project tokens holder
          pcountAllScriptInputs # txInputs #== 2
        , pcountScriptInputs # factoryValidatorHash # txInputs #== 1
        , pany
            # (ppaysAtleastToAddress # raisingCs # raisingTn # owedToDao # daoFeeReceiver)
            # txOutputs
        , pany
            # plam
              ( \o -> pletFields @["address", "value"] o \oF ->
                  (oF.address #== owner)
                    #&& pand'List
                      [ pif
                          (pisAda # raisingCs)
                          (pvalueOf # oF.value # raisingCs # raisingTn #>= returnedCollateral + owedToOwner)
                          ( pand'List
                              [ pvalueOf # oF.value # raisingCs # raisingTn #== owedToOwner
                              , pvalueOf # oF.value # padaSymbol # padaToken #>= returnedCollateral
                              ]
                          )
                      , -- ada && raising token
                        pcountOfUniqueTokensWithOverlap raisingCs oF.value #== 2
                      ]
              )
            # txOutputs
        , pcountOfUniqueTokens # vesting.value #== 2
        , pvalueOf # poolOutputF.value # projectCs # projectTn #== pvalueOf # ownInputValue # projectCs # projectTn
        , pvalueOf # poolOutputF.value # raisingCs # raisingTn #== owedToPool
        , correctPool
        , pdnothing #== pfield @"stakingCredential" # poolOutputF.address
        , vestingDatumF.beneficiary #== owner
        , vestingDatumF.vestingSymbol #== poolCs
        , vestingDatumF.vestingToken #== plpShareTn
        , vestingDatumF.totalVestingQty #== userGivenShares
        , vestingDatumF.vestingPeriodStart #== vestingPeriodStartTime
        , vestingDatumF.vestingPeriodEnd #== vestingPeriodStartTime + vestingPeriodDuration
        , vestingDatumF.firstUnlockPossibleAfter #== vestingPeriodStartTime + vestingPeriodDurationToFirstUnlock
        , vestingDatumF.totalInstallments #== vestingPeriodInstallments
        ]

projectTokensHolderValidator :: Term s (PTokensHolderFinalConfig :--> PValidator)
projectTokensHolderValidator = phoistAcyclic $ plam $ \cfg _dat redm' context -> unTermCont do
  (_dat, _) <- ptryFromC @(PAsData PUnit) _dat
  (redm, _) <- ptryFromC @(PAsData PTokenHolderRedeemerFinal) redm'
  pure $ popaque $ projectTokensHolderValidatorTyped # cfg # pfromData redm # context

projectTokensHolderScriptValidator :: TokensHolderFinalConfig -> Script
projectTokensHolderScriptValidator cfg = toScript (projectTokensHolderValidator # pconstant cfg)

projectTokensHolderScriptValidatorHash :: TokensHolderFinalConfig -> ScriptHash
projectTokensHolderScriptValidatorHash cfg = scriptHash (projectTokensHolderScriptValidator cfg)

projectTokensHolderScriptAddress :: TokensHolderFinalConfig -> Address
projectTokensHolderScriptAddress cfg = scriptHashToAddress (projectTokensHolderScriptValidatorHash cfg)

projectTokensHolderScript :: Script
projectTokensHolderScript = toScript projectTokensHolderValidator
