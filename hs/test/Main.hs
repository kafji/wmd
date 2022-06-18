module Main where

import Test.Tasty
import Wmd.Test.PercentEncodingTest
import Wmd.Test.SearchQueryTest

main :: IO ()
main = defaultMain tests

tests =
  testGroup
    "Tests"
    [ searchQueryTests
    , percentEncodingTests
    ]
