module Wmd.Config (
  getConfig,
) where

import Control.Exception (IOException, catch)
import Control.Monad (join)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import GHC.IO.Exception (IOException (ioe_description))
import System.Environment (getEnv)
import Wmd.Config.Parser (readConfig)
import Wmd.Config.Type (Config)
import Wmd.Type.Result (Result)
import Prelude hiding (readFile)

-- | Get @Config@.
getConfig :: IO (Result Config)
getConfig = fromEnvVar "WMD_CONFIG" <> fromFile "./wmd.toml"

{- |
Read Config from a file.

>>> fromFile "./not_exist"
Left "failed to read file `./not_exist`"
-}
fromFile :: FilePath -> IO (Result Config)
fromFile path = join <$> (readConfig <$>) <$> tryReadFile path
  where
    readFile' :: FilePath -> IO Text
    readFile' path = TextE.decodeUtf8 <$> ByteString.readFile path
    tryReadFile :: FilePath -> IO (Result Text)
    tryReadFile path =
      catch @IOException
        (Right <$> readFile' path)
        (\e -> pure $ Left $ "failed to read file `" <> (Text.pack path) <> "`, " <> (Text.pack . ioe_description) e)

{- |
Read Config from an environemnt variable.

>>> fromEnvVar "NOT_EXIST"
Left "failed to read env var `NOT_EXIST`"
-}
fromEnvVar :: String -> IO (Result Config)
fromEnvVar = (join <$>) . ((readConfig <$>) <$>) . ((Text.pack <$>) <$>) . tryGetEnv
  where
    tryGetEnv :: String -> IO (Result String)
    tryGetEnv name =
      catch @IOException
        (Right <$> getEnv name)
        (\e -> pure . Left $ "failed to read env var `" <> Text.pack name <> "`, " <> Text.pack (ioe_description e))
