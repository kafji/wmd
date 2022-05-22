module Wmd.Config (
  config,
  Config (..),
) where

import Control.Monad.Reader
import Data.Text (Text)
import Data.Text.IO
import Data.Toml.Parse
import Data.Toml.Parse (FromToml)
import Data.Vector qualified as V
import Text.Toml
import Wmd.Types
import Prelude hiding (readFile)

data Target = Target
  { prefix :: QueryPrefix
  , name :: Text
  , urlTemplate :: Text
  }
  deriving (Show, Eq)

data Config = Config
  { baseURL :: URL
  , targets :: [Target]
  }
  deriving (Show, Eq)

fromFile :: (MonadIO m, MonadFail m) => FilePath -> m Config
fromFile path = do
  file <- liftIO $ readFile path
  doc <- case parseTomlDoc path file of
    Left err -> fail $ show err
    Right doc -> pure doc
  case runParser doc tomlParser of
    Left err -> fail $ show err
    Right cfg -> pure cfg

searchTargetParser :: (Monad m, TomlParse m) => L Node -> m Target
searchTargetParser = undefined

tomlParser :: (Monad m, TomlParse m) => L Table -> m Config
tomlParser doc = Config <$> baseURL <*> targets
  where
    baseURL = pKey "base_url" doc >>= pStr
    targets = pKey "targets" doc >>= pTArray >>= undefined

config :: FilePath -> IO Config
config path = fromFile path
