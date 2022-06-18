module Main (
  main,
) where

import Test.Tasty (defaultMain, testGroup)
import Wmd.Test.HttpServerTest (httpServerTests)
import Wmd.Test.PercentEncodingTest (percentEncodingTests)
import Wmd.Test.SearchQueryTest (searchQueryTests)

main :: IO ()
main = defaultMain tests

tests =
  testGroup
    "tests"
    [ httpServerTests
    , searchQueryTests
    , percentEncodingTests
    ]
