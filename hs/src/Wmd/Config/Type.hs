module Wmd.Config.Type (
  Config (..),
  Target (..),
) where

import Data.Text (Text)
import Data.Vector (Vector)

-- | Configuration.
data Config = Config
  { base_url :: Text
  , targets :: Vector Target
  }
  deriving (Show, Eq)

data Target = Target
  { prefix :: Text
  , name :: Text
  , url_template :: Text
  }
  deriving (Show, Eq)
