{-# LANGUAGE NoImplicitPrelude #-}

module Integration.Vesting where

import GHC.Show (show)
import Integration.Compile (mkTypedValidatorPlutarchV2)
import Other.Vesting
import Plutus.Model
import PlutusTx.Prelude hiding (error)
import Prelude (error)

vestingValidator :: TypedValidator VestingDatum VestingRedeemer
vestingValidator = case mkTypedValidatorPlutarchV2 (pvalidateVestingScriptValidator) of
  Left err -> error $ show err
  Right val -> val
