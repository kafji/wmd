module Wmd.App (
  runApp,
) where

import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import Data.Either.Combinators (fromRight)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word16)
import Wmd.Cli (Flags (..), getFlags)
import Wmd.Config (getConfig)
import Wmd.Config.Type (Config (..))
import Wmd.HttpServer (ServerEnv (ServerEnv), runServer)
import Wmd.TargetUrl (urlFactory)
import Wmd.Type.SearchTarget (SearchTarget, fromConfig)

data AppEnv = AppEnv
  { serverPort :: Word16
  , urlTargets :: Vector SearchTarget
  }
  deriving (Show)

type AppM a = ReaderT AppEnv IO a

appEnv :: Flags -> Config -> AppEnv
appEnv flags config = AppEnv flags.port (Vector.map fromConfig config.targets)

app :: AppM ()
app = do
  port <- asks serverPort
  targets <- asks urlTargets
  lift $ runServer (ServerEnv port (urlFactory targets) (targets))

runApp :: IO ()
runApp = do
  !flags <- getFlags
  !config <- fromRight (error "failed to get config") <$> getConfig
  let env = appEnv flags config
  runReaderT app env
