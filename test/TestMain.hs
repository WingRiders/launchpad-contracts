module TestMain (main) where

import Data.Data
import Integration.Launchpad.Options (RunLimitTests, ThoroughTests)
import Integration.Tests
import System.IO
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Options
import Unit.Tests

tests :: TestTree
tests =
  testGroup
    "WingRiders"
    [integrationTestsLaunchpad, unitTestsLaunchpad, unitTestsOther]

main :: IO ()
main = do
  let options =
        [ Option (Proxy :: Proxy RunLimitTests)
        , Option (Proxy :: Proxy ThoroughTests)
        ]

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMainWithIngredients
    (includingOptions options : defaultIngredients)
    (localOption (HedgehogTestLimit (Just 250)) tests)
