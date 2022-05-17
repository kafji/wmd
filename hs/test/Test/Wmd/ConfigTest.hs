module Test.Wmd.ConfigTest where

import Test.Tasty
import Test.Tasty.HUnit
import Wmd.Config (config)

configTests :: TestTree
configTests =
  testGroup
    "Config"
    [ testCase "Parse toml" $ do
        cfg <- config "../wmd.toml"
        cfg @?= undefined
    ]
