module Wmd.Test.SearchQueryTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Wmd.SearchQuery (parseSearchQuery)
import Wmd.Type.SearchQuery (SearchQuery (SearchQuery))

searchQueryTests =
  testGroup
    "Parse search query"
    [ testCase "Complete query" $
        let expected = Right $ SearchQuery (Just "hg") "megaparsec"
            actual = parseSearchQuery "hg megaparsec"
         in expected @=? actual
    , testCase "Partial query" $
        let expected = Right $ SearchQuery Nothing "megaparsec"
            actual = parseSearchQuery "megaparsec"
         in expected @=? actual
    , testCase "Empty string" $
        Left "error" @=? parseSearchQuery ""
    ]
