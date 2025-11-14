{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Launchpad.RewardsFold where

-- TOOD: document checks for oil for two outputs that comes from collateral

import Data.Functor (($>))
import Launchpad.Constants
import Launchpad.Types
import Plutarch
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Extra.Value (padaOf)
import Plutarch.Lift
import Plutarch.Maybe
import Plutarch.Mint.Util
import Plutarch.PlutusScript
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Types.Classes
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

{- | Parameters of the Rewards Fold Validator

     The correctness of the values that parametrize the script is checked on the backend.
     For increased transparency, there are specific values of the individual parameters listed in the transaction metadata.

     It is in user's best interest to check if the parameters correspond to the ones provided in metadata, especially if not relying on solution's BE to interact with the contracts.
-}
data RewardsFoldConfig = RewardsFoldConfig
  { starter :: TxOutRef
  , owner :: Address
  , nodeSymbol :: CurrencySymbol
  , rewardsFoldPolicy :: CurrencySymbol
  , rewardsHolderValidatorHash :: ScriptHash
  , finalProjectTokensHolderValidatorHash :: ScriptHash
  , firstProjectTokensHolderValidatorHash :: ScriptHash
  , projectTokensHolderPolicy :: CurrencySymbol
  , projectSymbol :: CurrencySymbol
  , projectToken :: TokenName
  , raisingSymbol :: CurrencySymbol
  , raisingToken :: TokenName
  , presaleTierCs :: CurrencySymbol
  , tokensToDistribute :: Integer
  , withdrawalEndTime :: POSIXTime
  , oilAda :: Integer
  , commitFoldFeeAda :: Integer
  , -- if 0, only sundae pool is created
    -- if 10_000, only wr pool is created
    -- if 0 < splitBps < 10_000, splitBps determines what goes to Wr, the rest goes to Sundae
    -- NOTE: we don't ensure it's in the [0, 10_000] in the contracts, it's left to off-chain config creation
    splitBps :: Integer
  , daoFeeUnits :: Integer
  , daoFeeBase :: Integer
  , daoFeeReceiver :: Address
  , raisedTokensPoolPartPercentage :: Integer
  , collateral :: Integer
  }
  deriving (Show, Eq, Ord, Generic)

PlutusTx.makeIsDataIndexed ''RewardsFoldConfig [('RewardsFoldConfig, 0)]
PlutusTx.makeLift ''RewardsFoldConfig

data PRewardsFoldConfig (s :: S)
  = PRewardsFoldConfig
      ( Term
          s
          ( PDataRecord
              [ "starter" ':= PTxOutRef
              , "owner" ':= PAddress
              , "nodeSymbol" ':= PCurrencySymbol
              , "rewardsFoldPolicy" ':= PCurrencySymbol
              , "rewardsHolderValidatorHash" ':= PScriptHash
              , "finalProjectTokensHolderValidatorHash" ':= PScriptHash
              , "firstProjectTokensHolderValidatorHash" ':= PScriptHash
              , "projectTokensHolderPolicy" ':= PCurrencySymbol
              , "projectSymbol" ':= PCurrencySymbol
              , "projectToken" ':= PTokenName
              , "raisingSymbol" ':= PCurrencySymbol
              , "raisingToken" ':= PTokenName
              , "presaleTierCs" ':= PCurrencySymbol
              , "tokensToDistribute" ':= PInteger
              , "withdrawalEndTime" ':= PPOSIXTime
              , "oilAda" ':= PInteger
              , "commitFoldFeeAda" ':= PInteger
              , "splitBps" ':= PInteger
              , "daoFeeUnits" ':= PInteger
              , "daoFeeBase" ':= PInteger
              , "daoFeeReceiver" ':= PAddress
              , "raisedTokensPoolPartPercentage" ':= PInteger
              , "collateral" ':= PInteger
              ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow, PIsData, PDataFields)

instance DerivePlutusType PRewardsFoldConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRewardsFoldConfig where
  type PLifted PRewardsFoldConfig = RewardsFoldConfig

deriving via
  (DerivePConstantViaData RewardsFoldConfig PRewardsFoldConfig)
  instance
    (PConstantDecl RewardsFoldConfig)

rewardsFoldValidator :: Term s (PRewardsFoldConfig :--> PValidator)
rewardsFoldValidator = plam \cfg datum redeemer context ->
  let datum' = ptryFrom @PRewardsFoldDatum datum fst
      redeemer' = ptryFrom @PRewardsFoldRedeemer redeemer fst
   in popaque (perrorIfFalse #$ pvalidateRewardsFold cfg datum' redeemer' context)

data PCalculatedRewards (s :: S) = PCalculatedRewards
  { projectTokens :: Term s PInteger
  , returnedCommittedTokens :: Term s PInteger
  , takenCommittedTokens :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PCalculatedRewards where
  type DPTStrat _ = PlutusTypeScott

-- | Determines the expected rewards for a given node.
pdetermineRewards ::
  Term s (PMaybeData (PAsData PPOSIXTime)) ->
  Term s (PMaybeData (PAsData PNodeKey)) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  HRecOf PNode '["next", "key", "committed", "createdTime"] s ->
  Term s PCalculatedRewards
pdetermineRewards cutoffTime cutoffKey overcommitted totalToDistribute totalCommitted node =
  pmatch cutoffTime \case
    PDNothing _ -> pnormalRewards totalToDistribute totalCommitted node
    PDJust cTime' -> plet (pfromData (pfromData (pfield @"_0" # cTime'))) \cTime ->
      pcond
        [ (cTime #> node.createdTime, pnormalRewards totalToDistribute totalCommitted node)
        , (cTime #< node.createdTime, prefundRewards node)
        ]
        ( plet (pfromData (pfromDJust # cutoffKey)) \cKey -> plet (pfromData (pfromDJust # node.key)) \nodeKey ->
            pcond
              [ (pbuiltinPairLess # nodeKey # cKey, pnormalRewards totalToDistribute totalCommitted node)
              , (pbuiltinPairLess # cKey # nodeKey, prefundRewards node)
              ]
              (pcutoffRewards overcommitted totalToDistribute totalCommitted node)
        )

{- | Checks if the provided rewards utxo is adequate for the given node.
If it is, returns the checked rewards.
The rewards check enforces at least the minimum rewards, but it can be more.
-}
pcheckedRewards ::
  Term
    s
    ( (a :--> PNode :--> PCalculatedRewards)
        :--> PCurrencySymbol
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PScriptHash
        :--> PInteger
        :--> PNode
        :--> PValue 'Sorted 'Positive
        :--> PAddress
        :--> a
        :--> PMaybe PTxOut
        :--> PCalculatedRewards
    )
pcheckedRewards = phoistAcyclic $ plam \rewardsOf presaleSymbol committedSymbol committedToken projectSymbol projectToken rewardsHolderHash oilAda node nodeValue nodeAddress state reward ->
  unTermCont do
    rewardF <- pletFieldsC @'["value", "datum", "address"] (pfromJust # reward)
    rewardValue <- pletC rewardF.value
    rewardDatum <-
      pletFieldsC @'["owner", "projectSymbol", "projectToken", "raisingSymbol", "raisingToken"]
        (pfromPDatum @PRewardsHolderDatum # (ptryFromInlineDatum # rewardF.datum))
    nodeF <- pletFieldsC @'["next", "key", "committed", "createdTime"] node
    expectedRewards <- pletC (rewardsOf # state # node)
    expectedRewardsF <- pmatchC expectedRewards
    pguardC "L1" $
      pand'List
        [ ptraceIfFalse "L2" $ pisEqualScriptHashAddress # rewardsHolderHash # rewardF.address
        , ptraceIfFalse "L3" $ phaveSameStakingCredentials # rewardF.address # nodeAddress
        , ptraceIfFalse "L4" $ rewardDatum.owner #== pfromDJust # nodeF.key
        , ptraceIfFalse "L5" $ rewardDatum.projectSymbol #== projectSymbol
        , ptraceIfFalse "L6" $ rewardDatum.projectToken #== projectToken
        , ptraceIfFalse "L7" $ rewardDatum.raisingSymbol #== committedSymbol
        , ptraceIfFalse "L8" $ rewardDatum.raisingToken #== committedToken
        , pisRewardsValueCorrect
            presaleSymbol
            nodeValue
            rewardValue
            oilAda
            projectSymbol
            projectToken
            committedSymbol
            committedToken
            expectedRewardsF.projectTokens
            expectedRewardsF.returnedCommittedTokens
        ]
    pure expectedRewards

pisRewardsValueCorrect ::
  Term s PCurrencySymbol ->
  Term s (PValue 'Sorted anyAmount) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pisRewardsValueCorrect
  presaleCs
  nodeValue
  rewardValue
  oilAda
  projectSymbol
  projectToken
  committedSymbol
  committedToken
  expectedProjectTokens
  expectedCommittedTokens =
    pand'List
      [ -- The max is 4: ada + committed token + project token + tier token
        ptraceIfFalse "L9" $ pcountOfUniqueTokensWithOverlap committedSymbol rewardValue #<= 4
      , ptraceIfFalse "L10" $ pvalueOf # rewardValue # projectSymbol # projectToken #>= expectedProjectTokens
      , ptraceIfFalse "L11" $ plet (pvalueOf # rewardValue # committedSymbol # committedToken) \committed ->
          let adaValue = padaOf # rewardValue
           in pif
                (pisAda # committedSymbol)
                (committed #>= (expectedCommittedTokens + oilAda))
                (pand'List [committed #>= expectedCommittedTokens, adaValue #>= oilAda])
      , ptraceIfFalse "L12" $ plookupCurrency rewardValue presaleCs #== plookupCurrency nodeValue presaleCs
      ]

prewardsFormula :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
prewardsFormula = phoistAcyclic $ plam \totalToDistibute totalCommitted committed ->
  pdiv # (totalToDistibute * committed) # totalCommitted

pnormalRewards ::
  Term s PInteger ->
  Term s PInteger ->
  HRecOf PNode '["next", "key", "committed", "createdTime"] s ->
  Term s PCalculatedRewards
pnormalRewards totalToDistribute totalCommitted node = plet node.committed \committed ->
  pcon
    PCalculatedRewards
      { projectTokens = prewardsFormula # totalToDistribute # totalCommitted # committed
      , returnedCommittedTokens = 0
      , takenCommittedTokens = committed
      }

prefundRewards :: HRecOf PNode '["next", "key", "committed", "createdTime"] s -> Term s PCalculatedRewards
prefundRewards node =
  pcon
    PCalculatedRewards
      { projectTokens = 0
      , returnedCommittedTokens = node.committed
      , takenCommittedTokens = 0
      }

pcutoffRewards ::
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  HRecOf PNode '["next", "key", "committed", "createdTime"] s ->
  Term s PCalculatedRewards
pcutoffRewards overcommitted totalToDistribute totalCommitted node = plet (node.committed - overcommitted) \takenCommitted ->
  pcon
    PCalculatedRewards
      { projectTokens = prewardsFormula # totalToDistribute # totalCommitted # takenCommitted
      , returnedCommittedTokens = overcommitted
      , takenCommittedTokens = takenCommitted
      }

data PRewardsFoldAccumulator (s :: S) = PRewardsFoldAccumulator
  { nodeCount :: Term s PInteger
  -- ^ The number of processed nodes
  , committedPerTx :: Term s PInteger
  -- ^ The total used up committed funds per transaction
  , distributedPerTx :: Term s PInteger
  -- ^ The total distributed project funds per transaction
  , foldState :: Term s PRewardsFoldScott
  -- ^ The expected state of the fold utxo
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PRewardsFoldAccumulator where
  type DPTStrat _ = PlutusTypeScott

pnextRewardsState ::
  Term
    s
    ( (PNode :--> PValue 'Sorted 'Positive :--> PAddress :--> PRewardsFoldScott :--> PMaybe PTxOut :--> PCalculatedRewards)
        :--> (PRewardsFoldScott :--> PMaybeData (PAsData PNodeKey) :--> PRewardsFoldScott)
        :--> (PRewardsFoldScott :--> PMaybeData (PAsData PNodeKey))
        :--> PScriptHash
        :--> PRewardsFoldAccumulator
        :--> PTxOut
        :--> PMaybe PTxOut
        :--> PRewardsFoldAccumulator
    )
pnextRewardsState = phoistAcyclic $ plam \checkedRewardsOf withNext nextOf nodeHash acc node reward -> unTermCont do
  accF <- pmatchC acc
  nodeF <- pletFieldsC @'["value", "datum", "address"] node
  nodeDatum <- pletC (pfromPDatum @PNode # (ptryFromInlineDatum # nodeF.datum))
  nodeD <- pletFieldsC @'["next", "key", "committed", "createdTime"] nodeDatum
  checkedRewards <-
    pmatchC
      ( pif
          (pfromData nodeD.committed #== 0)
          (unTermCont (pguardC "separator nodes don't get rewards" (pisNothing # reward) $> pcon (PCalculatedRewards 0 0 0)))
          (checkedRewardsOf # nodeDatum # nodeF.value # nodeF.address # accF.foldState # reward)
      )
  pguardC "L13" $
    pand'List
      [ ptraceIfFalse "L14" $ nextOf # accF.foldState #== pfromData nodeD.key
      , ptraceIfFalse "L15" $ pisEqualScriptHashAddress # nodeHash # nodeF.address
      ]
  pure
    ( pcon
        accF
          { foldState = withNext # accF.foldState # nodeD.next
          , nodeCount = accF.nodeCount + 1
          , committedPerTx = accF.committedPerTx + checkedRewards.takenCommittedTokens
          , distributedPerTx = accF.distributedPerTx + checkedRewards.projectTokens
          }
    )

pnextRewardsStateFirstCome ::
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PScriptHash ->
  Term s PScriptHash ->
  Term s PInteger ->
  Term
    s
    ( PRewardsFoldAccumulator
        :--> PTxOut
        :--> PMaybe PTxOut
        :--> PRewardsFoldAccumulator
    )
pnextRewardsStateFirstCome
  presaleCs
  totalToDistribute
  committedSymbol
  committedToken
  projectSymbol
  projectToken
  nodeHash
  rewardsHolderHash
  oilAda =
    plet (plam (\state newNext -> pmatch state \s -> pcon @PRewardsFoldScott s {next = newNext})) \withNext ->
      pnextRewardsState
        # plam
          ( \nodeD nodeV nodeA state reward ->
              pcheckedRewards
                # plam
                  ( \state' node -> pmatch state' \s -> pletFields @'["next", "key", "committed", "createdTime"] node \n ->
                      pdetermineRewards s.cutoffTime s.cutoffKey s.overcommitted totalToDistribute s.committed n
                  )
                # presaleCs
                # committedSymbol
                # committedToken
                # projectSymbol
                # projectToken
                # rewardsHolderHash
                # oilAda
                # nodeD
                # nodeV
                # nodeA
                # state
                # reward
          )
        # withNext
        # plam (\state -> pmatch state \s -> s.next)
        # nodeHash

pemergencyWithdrawRewardsFold :: Term s PRewardsFoldConfig -> Term s PRewardsFoldDatum -> Term s PScriptContext -> Term s PBool
pemergencyWithdrawRewardsFold cfg datum context = unTermCont do
  cfgF <- pletFieldsC @'["withdrawalEndTime", "rewardsFoldPolicy"] cfg
  let commitFoldOwner = pfield @"commitFoldOwner" # datum
  contextF <- pletFieldsC @'["purpose", "txInfo"] context
  txInfoF <- pletFieldsC @'["outputs", "inputs", "mint", "signatories", "validRange"] contextF.txInfo
  PTimestamps lowerTime _ <- pmatchC (pfiniteTxValidityRangeTimestamps # txInfoF.validRange)
  selfOut <- pletC (pownInput # contextF.purpose # txInfoF.inputs)
  selfValidatorHash <- pletC (pgetValidatorHashFromScriptAddress #$ ptxOutAddress # selfOut)
  pure $
    pand'List
      [ ptraceIfFalse "L16" $ pcountAllScriptInputs # txInfoF.inputs #== 1
      , ptraceIfFalse "L17" $ pvalueOf # txInfoF.mint # cfgF.rewardsFoldPolicy # pscriptHashToTokenName selfValidatorHash #== -1
      , ptraceIfFalse "L18" $ plength # ((pto . pto) (pfromData txInfoF.mint)) #== 2
      , ptraceIfFalse "L19" $ pto (lowerTime - cfgF.withdrawalEndTime) #> pconstant emergencyWithdrawalPeriod
      , ptraceIfFalse "L20" $ ptxSignedBy # txInfoF.signatories # (paddressPubKeyCredential # commitFoldOwner)
      ]

{- | The rewards fold validator.

  Emergency withdrawal:
  - 1 rewards fold utxo is consumed, its validator is run
  - there is only one script utxo in the inputs
  - 1 rewards fold token is burned with its token name equal to the script hash of the rewards fold validator
  - the only currency symbol being burned is the rewards fold symbol (equal to 2, because mint always contains 0 ADA)
  - at least "emergencyWithdrawalPeriod" amount of time has passed since the withdrawalEndTime
  - the transaction is signed by the commit fold owner

  TODO: adjust the docs for 2 dexes
  Rewards fold:
  - the amount of burned node tokens is equal to the number of nodes in the inputs
  - there is a R 0,i rewards fold utxo in the inputs, its validator is run
  - at least 1 node is folded
  - R 0,i utxo has one rewards fold token with its token name equal to the script hash of the rewards fold validator
  - [Ai, ..., Aj] in the inputs is a valid linked list
  - there is a first project tokens holder T utxo in the inputs
  - each reward output has the corresponding owner's node key stored in its datum and stores the token asset classes used in the project
  - a new project tokens holder T' output utxo is created
  - the staking credential of T' is the same as of the T
  - the value of T' is the value of T minus the distributed project tokens plus the collected committed tokens
  - if the last processed node has a next key equal to nothing
    - the ADA value of R 0,i plus 2 ADA per processed node must be sent the commitFoldOwner address
    - the rewards fold token must be burned
    - the address of T' must be the final WingRiders version (ProjectTokensHolderFinal)
    - the datum of T' must be equal to WrTokensHolder
  - for every input node, there is a corresponding user reward utxo when node.committed /= 0
    - if the node.createdTime > just value of the cutoffTime
      OR the node.createdTime == cutoffTime but the node.key > cutoffKey
      then the reward utxo must have at least the value as big as the corresponding node element value with 2 oil ADA
    - if the node.createdTime < just value of the cutoffTime
      OR the cutoffTime is nothing
      OR the node.createdTime == cutoffTime but the node.key < cutoffKey,
      then the reward utxo must have a correct number of project tokens,
      2 oil ADA, and the tier token if it was contained in the node
    - if the node.createdTime == just value of the cutoffTime
      and the node.key == cutoffKey field,
      then the reward utxo must have a correct number of project tokens,
      it must have at least rewardsFold.overcommitted number of the committed tokens and 2 oil ADA,
      and the tier token if it was contained in the node
    - the staking credential of the reward output must be the same as of the corresponding node
  - if the last processed node.next is a just value
    - there is a R 0,t rewards fold utxo in the outputs with the rewards fold token
    - if there haven't been any raisingTokens collected yet expect 3 unique tokens else expect 4
    - the value of R 0,t is equal to the value of R 0,i plus 2 ADA per each processed node
    - the next field of R 0,t is the next of the last linked list element Aj
    - the cutoffKey field of R 0,t is equal to the cutoffKey field of R 0,i
    - the cutoffTime field of R 0,t is equal to the cutoffTime field of R 0,i
    - the overcommitted field of R 0,t is equal to the overcommitted field of R 0,i
    - the committed field of R 0,t is equal to the committed field of R 0,i
    - the commitFoldOwner field of R 0,t is equal to the commitFoldOwner field of R 0,i
    - the nodeScriptHash field of R 0,t is equal to the nodeScriptHash field of R 0,i
    - the address of T' must be the first version (ProjectTokensHolderFirst)
    - the datum of T' must be equal to the datum of T
-}
pvalidateRewardsFold :: Term s PRewardsFoldConfig -> Term s PRewardsFoldDatum -> Term s PRewardsFoldRedeemer -> Term s PScriptContext -> Term s PBool
pvalidateRewardsFold cfg datum redeemer' context = pmatch redeemer' \case
  PRewardsFoldEmergencyWithdrawal _ -> pemergencyWithdrawRewardsFold cfg datum context
  PRewardsFold redeemer -> unTermCont do
    cfgF <-
      pletFieldsC
        @'[ "nodeSymbol"
          , "rewardsFoldPolicy"
          , "rewardsHolderValidatorHash"
          , "finalProjectTokensHolderValidatorHash"
          , "firstProjectTokensHolderValidatorHash"
          , "projectTokensHolderPolicy"
          , "projectSymbol"
          , "projectToken"
          , "raisingSymbol"
          , "raisingToken"
          , "presaleTierCs"
          , "tokensToDistribute"
          , "oilAda"
          , "commitFoldFeeAda"
          , "splitBps"
          , "owner"
          , "daoFeeUnits"
          , "daoFeeBase"
          , "daoFeeReceiver"
          , "raisedTokensPoolPartPercentage"
          , "collateral"
          ]
        cfg

    redeemerF <-
      pletFieldsC
        @'[ "inputNodes"
          , "outputNodes"
          , "commitFoldCompensationIndex"
          , "inputTokensHolderIndex"
          , "inputRewardsFoldIndex"
          , "daoCompensationIndex"
          , "ownerCompensationIndex"
          ]
        redeemer
    inputNodes <- pletC redeemerF.inputNodes
    outputNodes <- pletC redeemerF.outputNodes

    contextF <- pletFieldsC @'["purpose", "txInfo"] context
    tx <- pletFieldsC @'["inputs", "outputs", "redeemers", "signatories", "mint", "datums"] contextF.txInfo
    inputs <- pletC tx.inputs
    outputs <- pletC tx.outputs

    pprojectSymbol <- pletC cfgF.projectSymbol
    pprojectToken <- pletC cfgF.projectToken
    pcommittedSymbol <- pletC cfgF.raisingSymbol
    pcommittedToken <- pletC cfgF.raisingToken
    projectTokensHolderFinalValidatorHash <- pletC cfgF.finalProjectTokensHolderValidatorHash
    projectTokensHolderFirstValidatorHash <- pletC cfgF.firstProjectTokensHolderValidatorHash
    projectTokensHolderCs <- pletC cfgF.projectTokensHolderPolicy

    splitBps <- pletC cfgF.splitBps

    PPair projectTokensHolderInInfo selfInInfo <-
      pmatchC (p2elemsAt # pfromData redeemerF.inputTokensHolderIndex # pfromData redeemerF.inputRewardsFoldIndex # inputs)

    selfInInfoF <- pletFieldsC @'["outRef", "resolved"] selfInInfo
    selfTxOut <- pletFieldsC @'["value", "address"] selfInInfoF.resolved
    selfCs <- pletC cfgF.rewardsFoldPolicy
    selfValidatorHash <- pletC (pgetValidatorHashFromScriptAddress # selfTxOut.address)

    projectTokensHolderInput <-
      pletFieldsC @'["datum", "value", "address"] (ptxInInfoResolved # projectTokensHolderInInfo)
    projectTokensHolderInputV <- pletC projectTokensHolderInput.value
    let inputHolderProjectTokens =
          pvalueOf
            # projectTokensHolderInputV
            # pprojectSymbol
            # pprojectToken
        inputHolderCommittedTokens =
          pvalueOf
            # projectTokensHolderInputV
            # pcommittedSymbol
            # pcommittedToken
        inputHolderAda = padaOf # projectTokensHolderInputV

    self <- pletC . toScott $ datum
    selfF <- pmatchC self

    resultAcc <-
      pmatchC
        ( pfoldInputsWithOutput
            (pcon (PRewardsFoldAccumulator 0 0 0 self))
            inputs
            outputs
            inputNodes
            outputNodes
            ( pnextRewardsStateFirstCome
                cfgF.presaleTierCs
                cfgF.tokensToDistribute
                pcommittedSymbol
                pcommittedToken
                pprojectSymbol
                pprojectToken
                selfF.nodeScriptHash
                cfgF.rewardsHolderValidatorHash
                cfgF.oilAda
            )
        )
    expectedStateF <- pmatchC resultAcc.foldState

    let selfAda = padaOf # selfTxOut.value
    -- Either the expected fold output ada value or the commit fold compensation
    expectedOutAda <- pletC (selfAda + cfgF.commitFoldFeeAda * resultAcc.nodeCount)

    pure $
      pand'List
        [ -- All nodes are processed + the rewards fold input + the project tokens holder input
          ptraceIfFalse "L21" $ pcountAllScriptInputs # inputs #== resultAcc.nodeCount + 2
        , ptraceIfFalse "L22" $ pvalueOf # tx.mint # cfgF.nodeSymbol # pscriptHashToTokenName selfF.nodeScriptHash #== ((-1) * resultAcc.nodeCount)
        , ptraceIfFalse "L23" $ resultAcc.nodeCount #> 0
        , ptraceIfFalse "L24" $ selfInInfoF.outRef #== pownRef contextF.purpose
        , ptraceIfFalse "L25" $ pvalueOf # selfTxOut.value # selfCs # pscriptHashToTokenName selfValidatorHash #== 1
        , ptraceIfFalse "L26" $ pisEqualScriptHashAddress # projectTokensHolderFirstValidatorHash # projectTokensHolderInput.address
        , pif
            (pisDNothing # expectedStateF.next)
            ( pcheckLastRewardsFold
                splitBps
                selfValidatorHash
                selfCs
                redeemerF.outputNodes
                (pfromData redeemerF.commitFoldCompensationIndex)
                (pfromData redeemerF.daoCompensationIndex)
                (pfromData redeemerF.ownerCompensationIndex)
                pcommittedSymbol
                pcommittedToken
                pprojectSymbol
                pprojectToken
                tx.mint
                outputs
                resultAcc
                selfF.commitFoldOwner
                expectedOutAda
                projectTokensHolderInput.address
                projectTokensHolderFinalValidatorHash
                projectTokensHolderFirstValidatorHash
                projectTokensHolderCs
                inputHolderCommittedTokens
                inputHolderProjectTokens
                (cfgF.daoFeeReceiver, cfgF.daoFeeUnits, cfgF.daoFeeBase)
                (cfgF.raisedTokensPoolPartPercentage, cfgF.collateral)
                cfgF.oilAda
                cfgF.owner
            )
            ( pcheckMiddleRewardsFold
                selfValidatorHash
                selfCs
                outputs
                pcommittedSymbol
                pcommittedToken
                pprojectSymbol
                pprojectToken
                (fromScott resultAcc.foldState)
                resultAcc
                expectedOutAda
                projectTokensHolderInput.datum
                projectTokensHolderInput.address
                projectTokensHolderFirstValidatorHash
                projectTokensHolderCs
                inputHolderAda
                inputHolderCommittedTokens
                inputHolderProjectTokens
            )
        ]

pcheckLastRewardsFold ::
  Term s PInteger ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s (PBuiltinList PTxOut) ->
  PRewardsFoldAccumulator s ->
  Term s PAddress ->
  Term s PInteger ->
  Term s PAddress ->
  Term s PScriptHash ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PInteger ->
  (Term s PAddress, Term s PInteger, Term s PInteger) ->
  (Term s PInteger, Term s PInteger) ->
  Term s PInteger ->
  Term s PAddress ->
  Term s PBool
pcheckLastRewardsFold
  splitBps
  selfValidatorHash
  selfCs
  outputNodesIndices
  commitFoldCompensationIndex
  daoCompensationIndex
  ownerCompensationIndex
  pcommittedSymbol
  pcommittedToken
  pprojectSymbol
  pprojectToken
  mint
  outputs
  resultAcc
  commitFoldOwner
  expectedOutAda
  projectTokensHolderInputAddress
  projectTokensHolderFinalValidatorHash
  projectTokensHolderFirstValidatorHash
  projectTokensHolderCs
  inputHolderCommittedTokens
  inputHolderProjectTokens
  (daoFeeReceiver, daoFeeUnits, daoFeeBase)
  (raisedTokensPoolPartPercentage, returnedCollateral)
  oilAda
  launchOwner = unTermCont do
    -- The committed tokens from the first tokens holder are split between:
    --   dao (fee from (total - collateral if ada))
    --   launch owner (percentage of (total - dao - collateral if ada))
    -- The (rest - collateral if ada) is split between:
    --   Wr final project tokens holder if used (percentage of what's left)
    --   Sundae final project tokens holder if used (what's left - wr tokens holder)
    collateralCommittedOut <- pletC $ pif (pisAda # pcommittedSymbol) returnedCollateral 0

    -- NOTE: this includes oil if ada
    totalCommittedOut <- pletC $ inputHolderCommittedTokens + resultAcc.committedPerTx - collateralCommittedOut
    daoCommittedOut <- pletC $ pdiv # (daoFeeUnits * totalCommittedOut) # daoFeeBase
    restCommittedOut <- pletC $ totalCommittedOut - daoCommittedOut
    tokensHoldersCommittedOut <- pletC $ pdiv # (restCommittedOut * raisedTokensPoolPartPercentage) # 100
    launchOwnerCommittedOut <- pletC $ restCommittedOut - tokensHoldersCommittedOut
    wrHolderCommittedOut <- pletC $ pdivideCeil # (tokensHoldersCommittedOut * splitBps) # bpsScalingFactor
    let sundaeHolderCommittedOut = tokensHoldersCommittedOut - wrHolderCommittedOut

    -- The project tokens from the first tokens holder are split between:
    --   Wr final project tokens holder if used (percentage of total)
    --   Sundae final project tokens holder if used (the rest)
    totalProjectOut <- pletC $ inputHolderProjectTokens - resultAcc.distributedPerTx
    wrHolderProjectOut <- pletC $ pdivideCeil # (totalProjectOut * splitBps) # bpsScalingFactor
    let sundaeHolderProjectOut = totalProjectOut - wrHolderProjectOut

    -- Rewards fold utxo ada is transferred to the commit fold compensation

    -- The ada from the first tokens holder is split between:
    --   dao (oil)
    --   commit fold compensation (oil)
    --   Wr final project tokens holder if used
    --   Sundae final project tokens holder if used
    --   launch owner (collateral - used up oil)
    let daoAdaFromCollateral = oilAda
        -- an oilAda amount per used dex
        holdersAdaFromCollateral = pcond [(splitBps #== 0, oilAda), (splitBps #== bpsScalingFactor, oilAda)] (oilAda * 2)
        ownerAda = returnedCollateral - daoAdaFromCollateral - holdersAdaFromCollateral

        -- Called with the correct holder utxo
        isHolderOutputCorrect out' dex holderCommittedOut holderProjectOut = unTermCont do
          out <- pletFieldsC @'["address", "datum", "value"] out'
          outValue <- pletC out.value
          let outCommitted =
                pvalueOf
                  # outValue
                  # pcommittedSymbol
                  # pcommittedToken
              outProject =
                pvalueOf
                  # outValue
                  # pprojectSymbol
                  # pprojectToken
              outAda = padaOf # outValue

          pure $
            pand'List
              [ -- The datum is a (data Wr)/(data Sundae) = (data 0)/(data 1)
                ptraceIfFalse "L27" $
                  (ptryFromInlineDatum # out.datum)
                    #== (pcon . PDatum . pforgetData . pdata $ dex)
              , -- The staking address part is the preserved from the first project tokens holder
                ptraceIfFalse "L28" $ phaveSameStakingCredentials # projectTokensHolderInputAddress # out.address
              , -- The expected number of commited tokens is locked
                pif
                  (pisAda # pcommittedSymbol)
                  (ptraceIfFalse "L29" $ (holderCommittedOut + oilAda) #== outCommitted)
                  ( pand'List
                      [ ptraceIfFalse "L30" $ holderCommittedOut #== outCommitted
                      , -- Oil (this ensure all of the expected oil is locked in)
                        -- Note that ada is unbounded for launches collecting non-ada
                        -- but bounded for launches collecting ada as the committed tokens are checked exactly
                        ptraceIfFalse "L31" $ oilAda #<= outAda
                      ]
                  )
              , -- The expected number of project tokens is locked
                ptraceIfFalse "L32" $ holderProjectOut #== outProject
              , -- ada, committed and project tokens
                -- in a weird case any of the expected values are 0, we allow less tokens
                ptraceIfFalse "L33" $ pcountOfUniqueTokensWithOverlap pcommittedSymbol outValue #<= 3
              ]

    tokenHolders <-
      pletC
        ( pfilter
            # plam
              ( \o ->
                  ppaysToCredential # projectTokensHolderFinalValidatorHash # o
              )
            # outputs
        )

    pure $
      pand'List
        [ pcond
            [
              ( --  splitBps = 10_000 means everything goes to WingRiders
                splitBps #== bpsScalingFactor
              , ptraceIfFalse "L34" $
                  isHolderOutputCorrect
                    (passertSingleton "L35" # tokenHolders)
                    (pcon PWr)
                    wrHolderCommittedOut
                    wrHolderProjectOut
              )
            ,
              ( --  splitBps = 0 means everything goes to Sundae
                splitBps #== 0
              , ptraceIfFalse "L36" $
                  isHolderOutputCorrect
                    (passertSingleton "L37" # tokenHolders)
                    (pcon PSundae)
                    sundaeHolderCommittedOut
                    sundaeHolderProjectOut
              )
            ]
            ( -- In case we need both pools, we require strict ordering
              pmatch (passertDoubleton tokenHolders) \(PPair outWr outSundae) ->
                pand'List
                  [ ptraceIfFalse "L38" $ isHolderOutputCorrect outWr (pcon PWr) wrHolderCommittedOut wrHolderProjectOut
                  , ptraceIfFalse "L39" $ isHolderOutputCorrect outSundae (pcon PSundae) sundaeHolderCommittedOut sundaeHolderProjectOut
                  ]
            )
        , -- We burn the rewards fold tokens
          ptraceIfFalse "L40" $ pvalueOf # mint # selfCs # pscriptHashToTokenName selfValidatorHash #== -1
        , ptraceIfFalse "L41" $ pvalueOf # mint # projectTokensHolderCs # pscriptHashToTokenName projectTokensHolderFirstValidatorHash #== -1
        , -- The commit fold compensations can't be reused as a node compensation
          -- this check prevents a double satisfaction attack
          ptraceIfFalse "L42" $ pnot # (pelem # pdata commitFoldCompensationIndex # outputNodesIndices)
        , ptraceIfFalse "L43" $ pnot # (pelem # pdata daoCompensationIndex # outputNodesIndices)
        , ptraceIfFalse "L44" $ pnot # (pelem # pdata ownerCompensationIndex # outputNodesIndices)
        , ptraceIfFalse "L45" $ pnot # (daoCompensationIndex #== ownerCompensationIndex)
        , ptraceIfFalse "L46" $ pnot # (daoCompensationIndex #== commitFoldCompensationIndex)
        , ptraceIfFalse "L47" $ pnot # (ownerCompensationIndex #== commitFoldCompensationIndex)
        , -- the commit fold is compensated, oil comes from the rewards fold utxo
          pletFields @["value", "address"] (pelemAt # commitFoldCompensationIndex # outputs) \commitCompensation ->
            pand'List
              [ ptraceIfFalse "L48" $ padaOf # commitCompensation.value #>= expectedOutAda
              , ptraceIfFalse "L49" $ pcountOfUniqueTokens # commitCompensation.value #== 1
              , ptraceIfFalse "L50" $ pfromData commitCompensation.address #== commitFoldOwner
              ]
        , -- The DAO is compensated
          pletFields @["value", "address"] (pelemAt # daoCompensationIndex # outputs) \daoCompensation ->
            pand'List
              [ ptraceIfFalse "L51" $ daoCompensation.address #== daoFeeReceiver
              , ptraceIfFalse "L52" $ pvalueOf # daoCompensation.value # pcommittedSymbol # pcommittedToken #>= daoCommittedOut
              , ptraceIfFalse "L53" $ padaOf # daoCompensation.value #>= daoAdaFromCollateral
              , -- Ada and the committed token, there's an edge case where the committed is zero
                -- we allow up to 2 tokens
                ptraceIfFalse "L54" $ pcountOfUniqueTokensWithOverlap pcommittedSymbol daoCompensation.value #<= 2
              ]
        , -- The owner is compensated
          pletFields @["value", "address"] (pelemAt # ownerCompensationIndex # outputs) \ownerCompensation ->
            pand'List
              [ ptraceIfFalse "L55" $ ownerCompensation.address #== launchOwner
              , pif
                  (pisAda # pcommittedSymbol)
                  (ptraceIfFalse "L56" $ padaOf # ownerCompensation.value #>= ownerAda + launchOwnerCommittedOut)
                  ( pand'List
                      [ ptraceIfFalse "L57" $ pvalueOf # ownerCompensation.value # pcommittedSymbol # pcommittedToken #== launchOwnerCommittedOut
                      , ptraceIfFalse "L58" $ padaOf # ownerCompensation.value #>= ownerAda
                      ]
                  )
              , -- Ada and the committed token, there's an edge case where the committed is zero
                -- we allow up to 2 tokens
                ptraceIfFalse "L59" $ pcountOfUniqueTokensWithOverlap pcommittedSymbol ownerCompensation.value #<= 2
              ]
        ]

pcheckMiddleRewardsFold ::
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s (PBuiltinList PTxOut) ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PRewardsFoldDatum ->
  PRewardsFoldAccumulator s ->
  Term s PInteger ->
  Term s POutputDatum ->
  Term s PAddress ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pcheckMiddleRewardsFold
  selfValidatorHash
  selfCs
  outputs
  pcommittedSymbol
  pcommittedToken
  pprojectSymbol
  pprojectToken
  expectedOutD
  resultAcc
  expectedOutAda
  projectTokensHolderInputDatum
  projectTokensHolderInputAddress
  projectTokensHolderFirstValidatorHash
  projectTokensHolderCs
  inputHolderAda
  inputHolderCommittedTokens
  inputHolderProjectTokens = unTermCont do
    projectTokensHolderOutput <-
      pletFieldsC @'["address", "datum", "value"]
        ( passertSingleton "L60"
            #$ pfilter
            # plam
              ( \o ->
                  ppaysToCredential # projectTokensHolderFirstValidatorHash # o
              )
            # outputs
        )
    projectTokensHolderOutputV <- pletC projectTokensHolderOutput.value
    let outputHolderCommittedTokens =
          pvalueOf
            # projectTokensHolderOutputV
            # pcommittedSymbol
            # pcommittedToken
        outputHolderProjectTokens =
          pvalueOf
            # projectTokensHolderOutputV
            # pprojectSymbol
            # pprojectToken
        outputHolderAda = padaOf # projectTokensHolderOutputV
        outputHolderValidity =
          pvalueOf
            # projectTokensHolderOutputV
            # projectTokensHolderCs
            # pscriptHashToTokenName projectTokensHolderFirstValidatorHash
    foldOut <-
      pletFieldsC @'["datum", "value"]
        ( passertSingleSpecificInput "L61"
            # pid
            # selfValidatorHash
            # selfCs
            # pscriptHashToTokenName selfValidatorHash
            # outputs
        )
    foldOutD <- pletC (pfromPDatum @PRewardsFoldDatum # (ptryFromInlineDatum # foldOut.datum))

    pure $
      pand'List
        [ ptraceIfFalse "L62" $ foldOutD #== expectedOutD
        , ptraceIfFalse "L63" $ phaveSameStakingCredentials # projectTokensHolderInputAddress # projectTokensHolderOutput.address
        , ptraceIfFalse "L64" $ padaOf # foldOut.value #>= expectedOutAda
        , -- ada and the rewards fold token
          ptraceIfFalse "L65" $ pcountOfUniqueTokens # foldOut.value #== 2
        , ptraceIfFalse "L66" $ projectTokensHolderInputDatum #== projectTokensHolderOutput.datum
        , plet (inputHolderCommittedTokens + resultAcc.committedPerTx) \expectedCommittedTokens ->
            pand'List
              [ ptraceIfFalse "L67" $ expectedCommittedTokens #== outputHolderCommittedTokens
              , -- ada, committed (in case there are currently any) and project tokens, and the tokens holder token
                ptraceIfFalse "L68" $ pcountOfUniqueTokensWithOverlap pcommittedSymbol projectTokensHolderOutputV #== pif (expectedCommittedTokens #== 0) 3 4
              , ptraceIfFalse "L69" $ outputHolderValidity #== 1
              ]
        , ptraceIfFalse "L70" $ inputHolderProjectTokens - resultAcc.distributedPerTx #== outputHolderProjectTokens
        , ptraceIfFalse "L71" $ inputHolderAda #<= outputHolderAda
        ]

rewardsFoldScriptValidator :: RewardsFoldConfig -> Script
rewardsFoldScriptValidator cfg = toScript (rewardsFoldValidator # pconstant cfg)

rewardsFoldScriptValidatorHash :: RewardsFoldConfig -> ScriptHash
rewardsFoldScriptValidatorHash = scriptHash . rewardsFoldScriptValidator

rewardsFoldScriptAddress :: RewardsFoldConfig -> Address
rewardsFoldScriptAddress = scriptHashToAddress . rewardsFoldScriptValidatorHash

rewardsFoldScript :: Script
rewardsFoldScript = toScript rewardsFoldValidator
