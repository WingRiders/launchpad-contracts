module Integration.Compile where

import Data.Text (Text)
import Plutarch.Api.V2 qualified as V2
import Plutarch.PlutusScript (defaultConfig)
import Plutarch.Prelude (ClosedTerm)
import Plutus.Model.Validator (TypedPolicy, TypedValidator)
import Plutus.Model.Validator.V2 as Model.Validator.V2 (
  mkTypedPolicyPlutarch,
  mkTypedValidatorPlutarch,
 )

mkTypedValidatorPlutarchV2 :: ClosedTerm V2.PValidator -> Either Text (TypedValidator datum redeemer)
mkTypedValidatorPlutarchV2 = Model.Validator.V2.mkTypedValidatorPlutarch defaultConfig

mkTypedPolicyPlutarchV2 :: ClosedTerm V2.PMintingPolicy -> Either Text (TypedPolicy redeemer)
mkTypedPolicyPlutarchV2 = Model.Validator.V2.mkTypedPolicyPlutarch defaultConfig
