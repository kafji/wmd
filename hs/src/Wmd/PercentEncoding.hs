{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Wmd.PercentEncoding (
  encodeText,
  toText,
) where

import Data.Char (ord)
import Data.Function ((&))
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

newtype PercentEncoded = PercentEncoded Text
  deriving (Show, Eq)

toText :: PercentEncoded -> Text
toText (PercentEncoded x) = x

-- | Set of special characters. Taken from https://developer.mozilla.org/en-US/docs/Glossary/percent-encoding.
specialChars :: HashSet Char
specialChars =
  S.fromList
    [ ':'
    , '/'
    , '?'
    , '#'
    , '['
    , ']'
    , '@'
    , '!'
    , '$'
    , '&'
    , '\''
    , '('
    , ')'
    , '*'
    , '+'
    , ','
    , ';'
    , '='
    , '%'
    , ' '
    ]

encodeChar :: Char -> Text
encodeChar c = "%" <> ((T.toUpper . T.pack) (showHex (ord c) ""))

{- |
Encodes text to percent encoded.

>>> encodeText "+mtl reader"
PercentEncoded "%2Bmtl%20reader"
-}
encodeText :: Text -> PercentEncoded
encodeText s = PercentEncoded replaceSpecials
  where
    replaceSpecials = T.foldl' (\a -> ((a <>) . mapChar)) mempty s

    mapChar c =
      c
        & if c `S.member` specialChars
          then encodeChar
          else T.singleton
