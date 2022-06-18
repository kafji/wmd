module Wmd.TargetUrl (
  urlFactory,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Wmd.Type.SearchQuery (SearchQuery (..))
import Wmd.Type.SearchTarget (SearchTarget (prefix, urlTemplate))
import Wmd.Type.Url (Url)

urlFactory :: Vector SearchTarget -> SearchQuery -> Maybe Url
urlFactory ts = \q -> (\t -> applyTemplate t q.keywords) <$> findTarget ts q

applyTemplate :: SearchTarget -> Text -> Url
applyTemplate target keywords = Text.replace "{keywords}" keywords target.urlTemplate

findTarget :: Vector SearchTarget -> SearchQuery -> Maybe SearchTarget
findTarget ts q = Vector.find (\t -> pure t.prefix == q.prefix) ts
