module Wmd.Type.SearchTarget (
  SearchTarget (..),
  fromConfig,
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Wmd.Config.Type (Target (..))

data SearchTarget = SearchTarget
  { prefix :: Text
  , name :: Text
  , urlTemplate :: Text
  }
  deriving (Show, Eq, Generic)

fromConfig ::
  -- | Config's target.
  Target ->
  SearchTarget
fromConfig Target {..} = SearchTarget prefix name url_template
