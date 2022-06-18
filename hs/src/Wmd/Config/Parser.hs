-- | Config TOML parser.
module Wmd.Config.Parser (
  readConfig,
) where

import Control.Monad
import Data.Either.Combinators (fromRight, maybeToRight)
import Data.Functor ((<&>))
import Data.HashMap.Lazy qualified as HashMapL
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Text.Toml (Node (VString, VTArray), Table, parseTomlDoc)
import Wmd.Config.Type (Config (..), Target (..))
import Wmd.Type.Result (Result)

-- | Read Config from TOML.
readConfig :: Text -> Result Config
readConfig toml = do
  let toml' = fromRight (error "failed to parse toml") . parseTomlDoc mempty $ toml
      baseUrl = readBaseUrl toml'
      targets = readTargets toml'
  Right Config <*> (maybeToRight "missing `base_url`" baseUrl) <&> ($ targets)

readBaseUrl :: Table -> Maybe Text
readBaseUrl = join . (readTomlText <$>) . HashMapL.lookup "base_url"

readTargets :: Table -> Vector Target
readTargets table = do
  let a = (\xs -> Vector.map readTarget xs) <$> (join $ readTomlVectorMap <$> HashMapL.lookup "targets" table)
  maybe mempty id (join (sequence <$> a))

readTarget :: Table -> Maybe Target
readTarget table = do
  let prefix = join $ readTomlText <$> HashMapL.lookup "prefix" table
      name = join $ readTomlText <$> HashMapL.lookup "name" table
      urlTemplate = join $ readTomlText <$> HashMapL.lookup "url_template" table
  Target <$> prefix <*> name <*> urlTemplate

readTomlText :: Node -> Maybe Text
readTomlText (VString x) = Just x
readTomlText _ = Nothing

readTomlVectorMap :: Node -> Maybe (Vector Table)
readTomlVectorMap (VTArray x) = Just x
readTomlVectorMap _ = Nothing
