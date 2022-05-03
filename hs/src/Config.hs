module Config (getConfig) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data SearchTarget = SearchTarget
  { prefix :: Text
  }
  deriving (Show)

type SearchTargets = HashMap Text SearchTarget

data Config = Config
  { searchTargets :: SearchTargets
  }
  deriving (Show)

getConfig :: IO Config
getConfig = pure Config {searchTargets = mempty}
