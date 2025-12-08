module Other.Vesting where

import GHC.Generics hiding (S)
import Plutarch
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.TermCont
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import Plutarch.Util
import Plutus.Util (scriptHashToAddress)
import PlutusLedgerApi.V2
import PlutusTx qualified

data VestingDatum = VestingDatum
  { beneficiary :: Address
  -- ^
  --  Beneficiary of the vesting script.
  --  After sufficient time has passed, he is eligible to unlock [vestingAsset] tokens.
  --  A signature corresponding to the PubKeyHash is needed to unlock funds.
  , vestingSymbol :: CurrencySymbol
  -- ^
  --  The asset that is being vested in this script.
  --  It is subject to the vesting period.
  --  Any other asset can be unlocked by the beneficiary freely at any time.
  , vestingToken :: TokenName
  -- ^
  --  The asset that is being vested in this script.
  --  It is subject to the vesting period.
  --  Any other asset can be unlocked by the beneficiary freely at any time.
  , totalVestingQty :: Integer
  -- ^
  --  Total quantity of the [vestingAsset] at the time of the initial creation of this vesting UTxO.
  --  It is used to correctly compute how many tokens can be unlocked over the vesting period.
  , vestingPeriodStart :: POSIXTime
  -- ^
  --  Start of the vesting period.
  , vestingPeriodEnd :: POSIXTime
  -- ^
  --  End of the vesting period.
  --  All tokens can be unlocked after this timestamp.
  , firstUnlockPossibleAfter :: POSIXTime
  -- ^
  --  First unlock is possible after this timestamp.
  --  No tokens can be unlocked before this timestamp.
  --
  --  Note: This timestamp should be between [vestingPeriodStart] and [vestingPeriodEnd].
  , totalInstallments :: Integer
  -- ^
  --  Total number of theoretical unlock installments split evenly throughout the vesting period.
  --  Tokens can be unlocked at most this number of times.
  --
  --  This number does not consider and is independent of the setting of [firstUnlockPossibleAfter]. Installments that would
  --  take place before the [firstUnlockPossibleAfter] timestamp are not possible to be carried out, but still count towards
  --  the first possible unlock.
  --
  --  For example, setting [totalInstallments = 24, firstUnlockPossibleAfter = now + 6 months, vestingPeriodStart = now, vestingPeriodEnd = now + 2 years]
  --  would render an installment once per month over 2 years. The beneficiary is able to unlock first tokens in 6 months.
  --  At this time, it's already 6 installments in, so he takes 6/24 of the [totalVestingQty].
  , vestingMemo :: BuiltinByteString
  -- ^
  --  Additional details of the vesting contract, to be used for presentation purposes.
  }

PlutusTx.makeLift ''VestingDatum
PlutusTx.makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]
data PVestingDatum (s :: S)
  = PVestingDatum
      ( Term
          s
          ( PDataRecord
              '[ "beneficiary" ':= PAddress
               , "vestingSymbol" ':= PCurrencySymbol
               , "vestingToken" ':= PTokenName
               , "totalVestingQty" ':= PInteger
               , "vestingPeriodStart" ':= PPOSIXTime
               , "vestingPeriodEnd" ':= PPOSIXTime
               , "firstUnlockPossibleAfter" ':= PPOSIXTime
               , "totalInstallments" ':= PInteger
               , "vestingMemo" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PVestingDatum where
  type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PVestingDatum

data VestingRedeemer
  = -- |
    --  Unlocks (a portion of) already vested tokens and keeps the rest locked.
    PartialUnlock
  | -- |
    --  Fully unlocks all tokens. Assumes they are all vested already.
    FullUnlock

PlutusTx.makeLift ''VestingRedeemer
PlutusTx.makeIsDataIndexed ''VestingRedeemer [('PartialUnlock, 0), ('FullUnlock, 1)]

data PVestingRedeemer (s :: S)
  = PPartialUnlock (Term s (PDataRecord '[]))
  | PFullUnlock (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PVestingRedeemer where
  type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PVestingRedeemer

{- |
  This validates a partial unlock of a vesting UTxO.
  This unlocks a portion of vesting tokens. It is possible to unlock some tokens even before [vestingPeriodEnd].
  The quantity of tokens that can be unlocked increases linearily with time that has passed since [vestingPeriodStart].

  To discourage unlocking tiny token fractions too often, it is possible to unlock tokens in whole
  batches only. Quantity of tokens unlocked in a single batch is [totalVestingQty / totalInstallments].
  There are total of [totalInstallments] number of batches. It is possible to unlock multiple batches at once if
  they are fully vested.

  Checks:
   1. Vesting beneficiary needs to sign the transaction.
   2. Current time approximation is later than the [firstUnlockPossibleAfter] timestamp. It is possible to unlock tokens now.
   3. The unlock is actually a partial and not a full unlock. Some quantity remains in the new vesting UTxO.
      To fully unlock the vesting UTxO, use [FullUnlock] redeemer.
   4. Some >0 quantity was withdrawn from the vesting UTxO.
   5. All vested tokens in the number of whole batches are all unlocked and taken out.
      We use the transaction validity start timestamp to lower bound and approximate the current time.
      Based on the current time, we count the number of remaining [futureInstallments] and make sure that exactly that
      many tokens are left inside the new vesting UTxO.
   6. New vesting UTxO has the same datum (vesting parameters did not change).

  Note: To prevent double-satisfaction, we enforce 1 vesting input and 1 vesting output.
-}
pvalidateVestingPartialUnlock :: Term s PVestingDatum -> Term s PScriptContext -> Term s PBool
pvalidateVestingPartialUnlock datum'' context' = unTermCont $ do
  PVestingDatum datum' <- pmatchC datum''

  context <- pletFieldsC @'["txInfo", "purpose"] context'
  txInfo <- pletFieldsC @'["outputs", "inputs", "signatories", "validRange"] context.txInfo
  txInfoInputs <- pletC txInfo.inputs

  let txOutputs = pfromData txInfo.outputs
      txInputs = pmap # ptxInInfoResolved # txInfoInputs
      scriptPurpose = context.purpose
      ownVestingInput = pownInput # scriptPurpose # txInfoInputs

  ownValidatorHash <- pletC $ pgetValidatorHashFromScriptAddress #$ ptxOutAddress # ownVestingInput

  let allVestingInputs = pfindScriptOutputs # ownValidatorHash # txInputs
      allVestingOutputs = pfindScriptOutputs # ownValidatorHash # txOutputs

  datum <-
    pletFieldsC
      @'[ "beneficiary"
        , "vestingSymbol"
        , "vestingToken"
        , "totalVestingQty"
        , "vestingPeriodStart"
        , "vestingPeriodEnd"
        , "firstUnlockPossibleAfter"
        , "totalInstallments"
        ]
      datum'

  let signatories = txInfo.signatories
      ctxTxInfoValidRange = pfromData txInfo.validRange

  beneficiary <- pletC datum.beneficiary

  let totalVestingQty = datum.totalVestingQty

  vestingSymbol <- pletC datum.vestingSymbol
  vestingToken <- pletC datum.vestingToken

  let vestingPeriodStart = pfromData $ datum.vestingPeriodStart
      vestingPeriodEnd = pfromData $ datum.vestingPeriodEnd

  totalInstallments <- pletC datum.totalInstallments

  currentTimeApproximation <- pletC $ plowerBoundCurrentTimeApproximation # ctxTxInfoValidRange

  let firstUnlockPossibleAfter = pfromData datum.firstUnlockPossibleAfter

  sVestingInput <- pletC $ passertSingleton "v single input" # allVestingInputs
  sVestingOutput <- pletC $ passertSingleton "v single output" # allVestingOutputs

  PPair oldVestingValue oldVestingDatum <- pmatchC sVestingInput
  PPair newVestingValue newVestingDatum <- pmatchC sVestingOutput

  let oldRemainingQty = pvalueOf # oldVestingValue # vestingSymbol # vestingToken

  newRemainingQty <- pletC $ pvalueOf # newVestingValue # vestingSymbol # vestingToken
  vestEndPeriod <- pletC $ pto vestingPeriodEnd

  let vestingPeriodLength = passertPositive # vestEndPeriod - pto vestingPeriodStart
      vestingTimeRemaining = passertPositive # vestEndPeriod - pto currentTimeApproximation
      timeBetweenTwoInstallments = passertPositive #$ pdivideCeil # vestingPeriodLength # totalInstallments
      futureInstallments = passertPositive #$ pdivideCeil # vestingTimeRemaining # timeBetweenTwoInstallments

      expectedRemainingQty =
        pdivideCeil # (futureInstallments * passertPositive # totalVestingQty) #$ passertPositive # totalInstallments

      beneficiaryHash = paddressPubKeyCredential #$ beneficiary

      signedByBeneficiary = ptxSignedByPkh # pdata beneficiaryHash # signatories
      firstUnlockPossible = firstUnlockPossibleAfter #< currentTimeApproximation
      isPartialUnlock = 0 #< newRemainingQty
      withdrawnSomething = newRemainingQty #< oldRemainingQty
      unlockingAllVested = expectedRemainingQty #== newRemainingQty
      datumNotChanged = oldVestingDatum #== newVestingDatum

  pure . pand'List $
    [ ptraceIfFalse "vPU signed by beneficiary" signedByBeneficiary
    , ptraceIfFalse "vPU first unlock possible" firstUnlockPossible
    , ptraceIfFalse "vPU is partial unlock" isPartialUnlock
    , ptraceIfFalse "vPU withdrawn something" withdrawnSomething
    , ptraceIfFalse "vPU unlocking all vested" unlockingAllVested
    , ptraceIfFalse "vPU datum not changed" datumNotChanged
    ]

{- |
  This validates a full unlock of a vesting UTxO.
   1. Vesting beneficiary needs to sign the transaction.
   2. Vesting period end is in the past.
      We use the transaction validity start timestamp to lower bound and approximate the current time.
-}
pvalidateVestingFullUnlock :: Term s PVestingDatum -> Term s PScriptContext -> Term s PBool
pvalidateVestingFullUnlock datum'' context = unTermCont $ do
  PVestingDatum datum' <- pmatchC datum''
  datum <- pletFieldsC @'["beneficiary", "vestingPeriodEnd"] datum'
  txInfo <- pletFieldsC @'["signatories", "validRange"] $ pfield @"txInfo" # context

  let beneficiary = datum.beneficiary
      vestingPeriodEnd = datum.vestingPeriodEnd
      signatories = txInfo.signatories
      txInfoValidRange = txInfo.validRange
      currentTimeApproximation = plowerBoundCurrentTimeApproximation # txInfoValidRange
      beneficiaryHash = paddressPubKeyCredential #$ pfromData beneficiary
      signedByBeneficiary = ptxSignedByPkh # pdata beneficiaryHash # signatories
      vestingPeriodEnded = vestingPeriodEnd #< currentTimeApproximation

  pure . pand'List $
    [ ptraceIfFalse "vFU signed by beneficiary" signedByBeneficiary
    , ptraceIfFalse "vFU vesting period ended" vestingPeriodEnded
    ]

{- |
  This validates the spending of vesting UTxOs.
  Based on the redeemer, it redirects the logic to either:
   1. Unlocks (a portion of) already vested tokens and keeps the rest locked.
   2. Fully unlock all tokens. Assumes they are all vested already.
-}
pvalidateVestingScript ::
  Term s PVestingDatum -> Term s PVestingRedeemer -> Term s PScriptContext -> Term s PUnit
pvalidateVestingScript datum redeemer context =
  perrorIfFalse #$ pmatch redeemer $ \case
    PPartialUnlock _ -> pvalidateVestingPartialUnlock datum context
    PFullUnlock _ -> pvalidateVestingFullUnlock datum context

pvalidateVestingScriptValidator :: Term s PValidator
pvalidateVestingScriptValidator = plam $ \rawDatum rawRedeemer ctx ->
  let datum = ptryFrom rawDatum fst
      redeemer = ptryFrom rawRedeemer fst
   in popaque $ pvalidateVestingScript datum redeemer ctx

vestingScriptValidator :: Script
vestingScriptValidator = toScript pvalidateVestingScriptValidator

vestingScriptAddress :: Address
vestingScriptAddress = scriptHashToAddress vestingScriptValidatorHash

vestingScriptValidatorHash :: ScriptHash
vestingScriptValidatorHash = scriptHash vestingScriptValidator
