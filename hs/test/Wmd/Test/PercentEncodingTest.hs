module Wmd.Test.PercentEncodingTest (
  percentEncodingTests,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Wmd.PercentEncoding

percentEncodingTests :: TestTree
percentEncodingTests =
  testGroup
    "percent encoding"
    [ testGroup
        "encode text"
        [ testCase "encode plus and space" $ "%2Bmtl%20reader" @=? (toText $ encodeText "+mtl reader")
        , testCase "empty string" $ "" @=? (toText $ encodeText "")
        ]
    ]
