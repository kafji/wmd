module Wmd.Type.SearchQuery (
  SearchQuery (..),
) where

import Data.Text (Text)

data SearchQuery = SearchQuery
  { prefix :: Maybe Text
  , keywords :: Text
  }
  deriving (Show, Eq)
