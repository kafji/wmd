{-# LANGUAGE OverloadedStrings #-}

module Test.Wmd.SearchQuery where

import Test.Tasty
import Test.Tasty.HUnit
import Wmd.SearchQuery

searchQueryTests =
  testGroup
    "Parse search query"
    [ testCase "Complete query" $
        let expected = Right $ Query (Just "hg") "megaparsec"
            actual = parseSearchQuery "hg megaparsec"
         in expected @=? actual
    , testCase "Partial query" $
        let expected = Right $ Query Nothing "megaparsec"
            actual = parseSearchQuery "megaparsec"
         in expected @=? actual
    , testCase "Empty string" $
        Left "error" @=? parseSearchQuery ""
    ]
