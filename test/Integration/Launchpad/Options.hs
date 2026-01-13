module Integration.Launchpad.Options (RunLimitTests (..), ThoroughTests (..)) where

import Test.Tasty.Options

newtype RunLimitTests = RunLimitTests Bool
  deriving (Eq, Show, Ord)

instance IsOption RunLimitTests where
  defaultValue = RunLimitTests False
  parseValue = fmap RunLimitTests . safeRead
  optionName = pure "run-limit-tests"
  optionHelp = pure "Run the tests which check the upper limits of the system"

newtype ThoroughTests = ThoroughTests Bool
  deriving (Eq, Show, Ord)

instance IsOption ThoroughTests where
  defaultValue = ThoroughTests False
  parseValue = fmap ThoroughTests . safeRead
  optionName = pure "thorough"
  optionHelp = pure "Run a lot more tests, taking a lot more time and resources"
