module Wmd.Test.SearchQueryTest (
  searchQueryTests,
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Wmd.SearchQuery (parseSearchQuery)
import Wmd.Type.SearchQuery (SearchQuery (SearchQuery))

searchQueryTests :: TestTree
searchQueryTests =
  testGroup
    "parse search query"
    [ testCase "complete query" $
        let expected = Right $ SearchQuery (Just "hg") "megaparsec"
            actual = parseSearchQuery "hg megaparsec"
         in expected @=? actual
    , testCase "partial query" $
        let expected = Right $ SearchQuery Nothing "megaparsec"
            actual = parseSearchQuery "megaparsec"
         in expected @=? actual
    , testCase "empty string" $
        Left "error" @=? parseSearchQuery ""
    ]
