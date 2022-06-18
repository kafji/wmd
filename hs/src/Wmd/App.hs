module Wmd.App (
  runApp,
) where

import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import Data.Either.Combinators (fromRight)
import Data.Text.IO qualified as TextIO
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word16)
import Network.Wai.Handler.Warp qualified as Warp
import TextShow (TextShow (showt))
import Wmd.Cli (Flags (..), getFlags)
import Wmd.Config (getConfig)
import Wmd.Config.Type (Config (..))
import Wmd.HttpServer (ServerEnv (ServerEnv), getServer)
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
  server <- lift $ getServer (ServerEnv (urlFactory targets) (targets))
  lift $ TextIO.putStrLn $ "starting server at port: " <> showt port
  lift $ Warp.run (fromIntegral port) server

runApp :: IO ()
runApp = do
  !flags <- getFlags
  !config <- fromRight (error "failed to get config") <$> getConfig
  let env = appEnv flags config
  runReaderT app env
