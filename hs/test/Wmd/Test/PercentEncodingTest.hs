module Wmd.Test.PercentEncodingTest where

import Test.Tasty
import Test.Tasty.HUnit
import Wmd.PercentEncoding

percentEncodingTests =
  testGroup
    "Percent encoding"
    [ testGroup
        "Encode text"
        [ testCase "Encode plus and space" $ "%2Bmtl%20reader" @=? (toText $ encodeText "+mtl reader")
        , testCase "Empty string" $ "" @=? (toText $ encodeText "")
        ]
    ]
