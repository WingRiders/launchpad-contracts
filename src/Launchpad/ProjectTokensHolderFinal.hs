{-# LANGUAGE BlockArguments #-}

module Launchpad.ProjectTokensHolderFinal (
  projectTokensHolderScriptValidator,
  projectTokensHolderScriptValidatorHash,
  projectTokensHolderScriptAddress,
  projectTokensHolderScript,
  projectTokensHolderValidator,
  TokensHolderFinalRedeemer (..),
  TokensHolderFinalConfig (..),
)
where

import Data.ByteString.Hash (blake2b_256)
import Data.Text.Encoding (encodeUtf8)
import Launchpad.Constants qualified as C
import Launchpad.PoolTypes
import Launchpad.Types
import Launchpad.Util (poolSundaeLpName, poolSundaeNftName)
import Other.Vesting (PVestingDatum (..))
import Plutarch
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Util
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
  , wrPoolSymbol :: CurrencySymbol
  , wrPoolValidatorHash :: ScriptHash
  , wrFactoryValidatorHash :: ScriptHash
  , sundaePoolScriptHash :: ScriptHash
  , sundaeFeeTolerance :: Integer
  , sundaeSettingsCurrencySymbol :: CurrencySymbol
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
              , "wrPoolSymbol" ':= PCurrencySymbol
              , "wrPoolValidatorHash" ':= PScriptHash
              , "wrFactoryValidatorHash" ':= PScriptHash
              , "sundaePoolScriptHash" ':= PScriptHash
              , "sundaeFeeTolerance" ':= PInteger
              , "sundaeSettingsCurrencySymbol" ':= PCurrencySymbol
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

{- | Share token names are unique per pool for WingRiders.
In V2 they are computed in the following way:
shareTokenName = blake2b (blake2b poolType <> blake2b aScale <> blake2b bScale <> blake2b assetA <> blake2b assetB)

Note that we don't support stableswap pools in the launchpad,
hence the pool type and the scales are statically known
-}
pwrShareTokenName ::
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PTokenName
pwrShareTokenName raisingSymbol raisingToken projectSymbol projectToken =
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

data TokensHolderFinalRedeemer
  = FailedFlow
  | NormalFlow
  deriving stock (Generic, Enum, Bounded)
  deriving (PlutusTx.ToData, PlutusTx.FromData) via (EnumIsData TokensHolderFinalRedeemer)

data PTokensHolderFinalRedeemer (s :: S)
  = PFailedFlow
  | PNormalFlow
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PTokensHolderFinalRedeemer where
  type DPTStrat _ = PlutusTypeEnumData

instance PTryFrom PData (PAsData PTokensHolderFinalRedeemer)

{- |
TODO: fix all the docs

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
  Term s PTokensHolderFinalConfig ->
  Term s PDex ->
  Term s PTokensHolderFinalRedeemer ->
  Term s PScriptContext ->
  Term s PBool
projectTokensHolderValidatorTyped cfg datum redeemer context = unTermCont do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] context
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "mint", "referenceInputs"] ctxF.txInfo

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
        , "wrPoolSymbol"
        , "wrPoolValidatorHash"
        , "wrFactoryValidatorHash"
        , "sundaePoolScriptHash"
        , "sundaeFeeTolerance"
        , "sundaeSettingsCurrencySymbol"
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

  numRaised <- pletC $ pvalueOf # ownInputValue # raisingCs # raisingTn

  pure $
    pand'List
      [ pmatch redeemer \case
          PNormalFlow ->
            pmatch datum \case
              PWr ->
                pvalidateNormalFlowWr
                  cfgF.owner
                  cfgF.wrFactoryValidatorHash
                  (cfgF.vestingValidatorHash, cfgF.vestingPeriodInstallments, cfgF.vestingPeriodDuration, cfgF.vestingPeriodDurationToFirstUnlock, cfgF.vestingPeriodStart)
                  (cfgF.wrPoolSymbol, cfgF.wrPoolValidatorHash)
                  (projectCs, projectTn)
                  (raisingCs, raisingTn)
                  numRaised
                  ownInputValue
                  txInputs
                  txOutputs
                  mint
              PSundae ->
                pvalidateNormalFlowSundae
                  cfgF.owner
                  (cfgF.vestingValidatorHash, cfgF.vestingPeriodInstallments, cfgF.vestingPeriodDuration, cfgF.vestingPeriodDurationToFirstUnlock, cfgF.vestingPeriodStart)
                  cfgF.sundaePoolScriptHash
                  cfgF.sundaeFeeTolerance
                  cfgF.sundaeSettingsCurrencySymbol
                  (projectCs, projectTn)
                  (raisingCs, raisingTn)
                  numRaised
                  ownInputValue
                  txInputs
                  txRefInputs
                  txOutputs
                  mint
          PFailedFlow ->
            pvalidateFailedFlow
              cfgF.daoFeeReceiver
              cfgF.sundaeSettingsCurrencySymbol
              cfgF.sundaeFeeTolerance
              cfgF.poolProofValidatorHash
              (projectCs, projectTn)
              (raisingCs, raisingTn)
              numRaised
              ownInputValue
              txInputs
              txOutputs
              txRefInputs
              datum
      ]

pvalidateNormalFlowSundae ::
  Term s PAddress ->
  (Term s PScriptHash, Term s PInteger, Term s PPOSIXTime, Term s PPOSIXTime, Term s PPOSIXTime) ->
  Term s PScriptHash ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PInteger ->
  Term s (PValue anyKey anyAmount) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PBool
pvalidateNormalFlowSundae
  owner
  (vestingValidatorHash, vestingPeriodInstallments, vestingPeriodDuration, vestingPeriodDurationToFirstUnlock, vestingPeriodStartTime)
  poolScriptHash
  feeTolerance
  settingsCurrencySymbol
  (projectCs, projectTn)
  (raisingCs, raisingTn)
  numRaised
  ownInputValue
  txInputs
  txReferenceInputs
  txOutputs
  mint = unTermCont do
    let owedToPool = numRaised

    let poolUTxO =
          passertSingleton "J1"
            #$ pfilter
            # plam (\o -> ppaysToCredential # poolScriptHash # o)
            # txOutputs
    poolOutputF <- pletFieldsC @["address", "datum", "value"] poolUTxO

    let poolDatum = pfromPDatum @PSundaePoolDatum #$ ptryFromInlineDatum # poolOutputF.datum
    pool <- pletFieldsC @'["identifier", "circulatingLp", "protocolFees"] poolDatum

    poolIdentifier <- pletC pool.identifier
    -- The pool minting policy allows setting the protocolFees above what's required
    -- by the Sundae settings, we require an exact match
    protocolFees <- pletC pool.protocolFees
    let poolCs = pcon . PCurrencySymbol . pto $ poolScriptHash
    plpShareTn <- pletC (poolSundaeLpName poolIdentifier)
    poolNft <- pletC (poolSundaeNftName poolIdentifier)

    -- All minted shares are given to the user
    -- The exact value is stored in the datum
    -- It's validated by the pool minting policy
    userGivenShares <- pletC pool.circulatingLp

    let vestingOutput =
          ptryFindOutputWithAsset
            # vestingValidatorHash
            # (pcon . PCurrencySymbol . pto $ poolScriptHash)
            # plpShareTn
            # userGivenShares
            # txOutputs
    vesting <- pletFieldsC @["datum", "value"] vestingOutput
    let vestingDatum = pfromPDatum @PVestingDatum #$ ptryFromInlineDatum # vesting.datum
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

    let settings = findSundaeSettingsDatum txReferenceInputs settingsCurrencySymbol

    -- NOTE: staking choice is restricted by the "settings" ref utxo, we don't check it
    pure . ptraceIfFalse "J2" $
      pand'List
        [ -- The tokens holder
          ptraceIfFalse "J3" $ pcountAllScriptInputs # txInputs #== 1
        , -- The Sundae pool creation fee is tolerated by the launch
          ptraceIfFalse "J4" $ protocolFees #<= feeTolerance
        , -- The deposited Sundae pool creation fee is the same as in the settings
          ptraceIfFalse "J5" $ protocolFees #== pfield @"poolCreationFee" # settings
        , ptraceIfFalse "J6" $ pcountOfUniqueTokens # vesting.value #== 2
        , -- The pool receives all project tokens, we don't need to check the datum assets, as the policy does that
          ptraceIfFalse "J7" $ pvalueOf # poolOutputF.value # projectCs # projectTn #== pvalueOf # ownInputValue # projectCs # projectTn
        , -- The pool receives all raising tokens or more, we don't need to check the datum assets, as the policy does that
          ptraceIfFalse "J8" $
            pif
              (pisAda # raisingCs)
              -- Please note that it's possible to add more ada and increase the project token price
              -- when the project is raising ada;
              -- That's done to allow finishing the launch when there's a pool creation fee
              -- and the utxo doesn't have enough ada to cover it;
              -- Note that in case the fee is above the tolerance, the failing flow must be used instead.
              (pvalueOf # poolOutputF.value # raisingCs # raisingTn #>= owedToPool)
              (pvalueOf # poolOutputF.value # raisingCs # raisingTn #== owedToPool)
        , -- The nft was minted, the policy will ensure it's stored in the pool
          ptraceIfFalse "J9" $ pvalueOf # mint # poolCs # poolNft #== 1
        , -- The vesting has correct datum
          ptraceIfFalse "J10" $ vestingDatumF.beneficiary #== owner
        , ptraceIfFalse "J11" $ vestingDatumF.vestingSymbol #== poolCs
        , ptraceIfFalse "J12" $ vestingDatumF.vestingToken #== plpShareTn
        , ptraceIfFalse "J13" $ vestingDatumF.totalVestingQty #== userGivenShares
        , ptraceIfFalse "J14" $ vestingDatumF.vestingPeriodStart #== vestingPeriodStartTime
        , ptraceIfFalse "J15" $ vestingDatumF.vestingPeriodEnd #== vestingPeriodStartTime + vestingPeriodDuration
        , ptraceIfFalse "J16" $ vestingDatumF.firstUnlockPossibleAfter #== vestingPeriodStartTime + vestingPeriodDurationToFirstUnlock
        , ptraceIfFalse "J17" $ vestingDatumF.totalInstallments #== vestingPeriodInstallments
        ]

-- NOTE: we need to validate it because no Sundae-side validation is being run when the fee is above the tolerance
findSundaeSettingsDatum :: Term s (PBuiltinList PTxInInfo) -> Term s PCurrencySymbol -> Term s PSundaeSettingsDatum
findSundaeSettingsDatum referenceInputs settingsCurrencySymbol = unTermCont do
  let settings =
        passertSingleton "J18"
          #$ pfilter
          # plam
            ( \o -> unTermCont do
                settingsOutput <- pletFieldsC @'["value", "datum"] $ ptxInInfoResolved # o
                pure (pvalueOf # settingsOutput.value # settingsCurrencySymbol # pconstant C.settingsNftName #== 1)
            )
          # referenceInputs
  settingsOutput <- pletFieldsC @'["value", "datum"] $ ptxInInfoResolved # settings
  settingsDatum <- pletC $ pfromPDatum #$ ptryFromInlineDatum # settingsOutput.datum
  pure settingsDatum

pvalidateFailedFlow ::
  Term s PAddress ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PScriptHash ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PInteger ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s PDex ->
  Term s PBool
pvalidateFailedFlow
  daoFeeReceiver
  settingsCurrencySymbol
  sundaeFeeTolerance
  poolProofValidatorHash
  (projectCs, projectTn)
  (raisingCs, raisingTn)
  numRaised
  ownInputValue
  txInputs
  txOutputs
  txRefInputs
  dex = unTermCont do
    let dexChecks = pmatch dex \case
          -- For Wr the failing happens when a pool already exists
          -- we validate it with a pool proof reference utxo
          PWr -> unTermCont do
            let poolProofTxIn =
                  passertSingleton "J19"
                    #$ pfilter
                    # plam
                      (\o -> ppaysToCredential # poolProofValidatorHash # (ptxInInfoResolved # o))
                    # txRefInputs
                poolProofOut = pfromData (ptxInInfoResolved # poolProofTxIn)
                poolProofDatum = pfromPDatum @PPoolProofDatum # (ptryFromInlineDatum #$ pfield @"datum" # poolProofOut)
            poolProofFields <- pletFieldsC @'["projectSymbol", "projectToken", "raisingSymbol", "raisingToken", "dex"] poolProofDatum
            pure $
              pand'List
                [ ptraceIfFalse "J20" $ poolProofFields.projectSymbol #== projectCs
                , ptraceIfFalse "J21" $ poolProofFields.projectToken #== projectTn
                , ptraceIfFalse "J22" $ poolProofFields.raisingSymbol #== raisingCs
                , ptraceIfFalse "J23" $ poolProofFields.raisingToken #== raisingTn
                , ptraceIfFalse "J24" $ poolProofFields.dex #== pcon PWr
                ]
          -- For Sundae the failing happens when the pool creation fee is above the tolerance
          -- NOTE: in case of a stables-collecting launch where the pool creation fee is above min ada
          --       the utxo won't have enough ada to pay for the fee
          --       it's still possible to finish with the normal flow by providing the fee externally
          --       it's explicitely not a failure scenario
          PSundae -> unTermCont do
            let settings = findSundaeSettingsDatum txRefInputs settingsCurrencySymbol
                poolCreationFee = pfield @"poolCreationFee" # settings
            pure $ ptraceIfFalse "J25" $ poolCreationFee #> sundaeFeeTolerance

    projectTokens <- pletC (pvalueOf # ownInputValue # projectCs # projectTn)
    let raisedToDao = numRaised
    pure . ptraceIfFalse "J27" . pand'List $
      [ -- The project tokens holder
        ptraceIfFalse "J28" $ pcountAllScriptInputs # txInputs #== 1
      , ptraceIfFalse "J29" $
          pany
            # plam
              ( \txOut -> unTermCont do
                  txoFields <- pletFieldsC @["address", "value"] txOut
                  txOutValue <- pletC txoFields.value
                  pure $
                    (txoFields.address #== daoFeeReceiver)
                      #&& pand'List
                        [ pvalueOf # txOutValue # raisingCs # raisingTn #== raisedToDao
                        , pvalueOf # txOutValue # projectCs # projectTn #== projectTokens
                        , -- In a weird case any the computed values is 0 we allow less tokens
                          pcountOfUniqueTokensWithOverlap raisingCs txOutValue #<= 3
                        ]
              )
            # txOutputs
      , dexChecks
      ]

pvalidateNormalFlowWr ::
  Term s PAddress ->
  Term s PScriptHash ->
  (Term s PScriptHash, Term s PInteger, Term s PPOSIXTime, Term s PPOSIXTime, Term s PPOSIXTime) ->
  (Term s PCurrencySymbol, Term s PScriptHash) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PInteger ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PBool
pvalidateNormalFlowWr
  owner
  factoryValidatorHash
  (vestingValidatorHash, vestingPeriodInstallments, vestingPeriodDuration, vestingPeriodDurationToFirstUnlock, vestingPeriodStartTime)
  (poolCs', poolValidatorHash)
  (projectCs, projectTn)
  (raisingCs, raisingTn)
  numRaised
  ownInputValue
  txInputs
  txOutputs
  mint = unTermCont do
    plpShareTn <- pletC $ pwrShareTokenName raisingCs raisingTn projectCs projectTn
    poolCs <- pletC poolCs'

    let mintedShares = pvalueOf # mint # poolCs # plpShareTn
        poolUTxO =
          ptryFindOutputWithAsset
            # poolValidatorHash
            # poolCs
            # pconstant C.lpValidityTokenName
            # 1
            # txOutputs

    poolOutputF <- pletFieldsC @["address", "value"] poolUTxO

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
    let vestingDatum = pfromPDatum @PVestingDatum #$ ptryFromInlineDatum # vesting.datum

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
    pure . ptraceIfFalse "J30" . pand'List $
      [ -- The factory and the tokens holder
        ptraceIfFalse "J31" $ pcountAllScriptInputs # txInputs #== 2
      , -- The factory is there
        ptraceIfFalse "J32" $ pcountScriptInputs # factoryValidatorHash # txInputs #== 1
      , -- The vesting has ada and shares
        ptraceIfFalse "J33" $ pcountOfUniqueTokens # vesting.value #== 2
      , -- The pool receives all project tokens, ensures the pool assets are correct as the factory checks token count
        ptraceIfFalse "J34" $ pvalueOf # poolOutputF.value # projectCs # projectTn #== pvalueOf # ownInputValue # projectCs # projectTn
      , -- The pool receives all raising tokens, ensures the pool assets are correct as the factory checks token count
        ptraceIfFalse "J35" $ pvalueOf # poolOutputF.value # raisingCs # raisingTn #== numRaised
      , -- The pool staking part is set to nothing
        ptraceIfFalse "J36" $ pdnothing #== pfield @"stakingCredential" # poolOutputF.address
      , -- The vesting has correct datum
        ptraceIfFalse "J37" $ vestingDatumF.beneficiary #== owner
      , ptraceIfFalse "J38" $ vestingDatumF.vestingSymbol #== poolCs
      , ptraceIfFalse "J39" $ vestingDatumF.vestingToken #== plpShareTn
      , ptraceIfFalse "J40" $ vestingDatumF.totalVestingQty #== userGivenShares
      , ptraceIfFalse "J41" $ vestingDatumF.vestingPeriodStart #== vestingPeriodStartTime
      , ptraceIfFalse "J42" $ vestingDatumF.vestingPeriodEnd #== vestingPeriodStartTime + vestingPeriodDuration
      , ptraceIfFalse "J43" $ vestingDatumF.firstUnlockPossibleAfter #== vestingPeriodStartTime + vestingPeriodDurationToFirstUnlock
      , ptraceIfFalse "J44" $ vestingDatumF.totalInstallments #== vestingPeriodInstallments
      ]

projectTokensHolderValidator :: Term s (PTokensHolderFinalConfig :--> PValidator)
projectTokensHolderValidator = plam $ \cfg dat' redm' context -> unTermCont do
  (dat, _) <- ptryFromC @(PAsData PDex) dat'
  (redm, _) <- ptryFromC @(PAsData PTokensHolderFinalRedeemer) redm'
  pure . popaque $ perrorIfFalse #$ projectTokensHolderValidatorTyped cfg (pfromData dat) (pfromData redm) context

projectTokensHolderScriptValidator :: TokensHolderFinalConfig -> Script
projectTokensHolderScriptValidator cfg = toScript (projectTokensHolderValidator # pconstant cfg)

projectTokensHolderScriptValidatorHash :: TokensHolderFinalConfig -> ScriptHash
projectTokensHolderScriptValidatorHash cfg = scriptHash (projectTokensHolderScriptValidator cfg)

projectTokensHolderScriptAddress :: TokensHolderFinalConfig -> Address
projectTokensHolderScriptAddress cfg = scriptHashToAddress (projectTokensHolderScriptValidatorHash cfg)

projectTokensHolderScript :: Script
projectTokensHolderScript = toScript projectTokensHolderValidator
