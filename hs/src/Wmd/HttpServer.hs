{- |
Defines HTTP server and its request handlers.
-}
module Wmd.HttpServer (
  ServerEnv (ServerEnv),
  getServer,
) where

import Control.Monad.Reader (
  ReaderT (runReaderT),
  asks,
 )
import Data.Text (Text)
import Data.Text.Lazy qualified as TextL
import Data.Vector (Vector)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans
import Wmd.Html (errorPage, homePage, prefixesPage)
import Wmd.SearchQuery (parseSearchQuery)
import Wmd.Type.SearchQuery (SearchQuery)
import Wmd.Type.SearchTarget (SearchTarget)

data ServerEnv = ServerEnv
  { createUrl :: SearchQuery -> Text
  , targets :: Vector SearchTarget
  }

type ServerM = ReaderT ServerEnv IO

type Handler e = (ScottyError e) => ActionT e ServerM ()

server :: ScottyT TextL.Text ServerM ()
server = do
  middleware logStdout
  mconcat
    [ get "/" getIndex
    , get "/search" getSearch
    , get "/prefixes" getPrefixes
    ]

getServer :: ServerEnv -> IO Application
getServer env = scottyAppT run server
  where
    run = flip runReaderT env

-- | Handles `GET /`.
getIndex :: Handler e
getIndex = html homePage

-- | Handles `GET /search`.
getSearch :: forall e. Handler e
getSearch =
  do
    q <- rescue (Just <$> param "q") (const (pure Nothing))
    maybe (errorPage' "missing `q` param") toTarget q
  where
    toTarget =
      either
        ( -- todo(kfj): log the actual error
          const (errorPage' "failed to parse search query")
        )
        redirectToTarget
        . parseSearchQuery
    redirectToTarget q = do
      url <- (\f -> f q) <$> asks createUrl
      redirect (TextL.fromStrict url)

-- | Handles `GET /prefixes`.
getPrefixes :: Handler e
getPrefixes = asks targets >>= html . prefixesPage

errorPage' :: (ScottyError e) => Text -> ActionT e ServerM ()
errorPage' msg = html (errorPage msg)
