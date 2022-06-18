{- |
Defines HTTP server and its request handlers.
-}
module Wmd.HttpServer (
  ServerEnv (ServerEnv),
  getServer,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Reader (
  ReaderT (runReaderT),
  asks,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Vector (Vector)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans
import Wmd.Html (homePage, prefixesPage, searchPage)
import Wmd.SearchQuery (parseSearchQuery)
import Wmd.Type.SearchQuery (SearchQuery)
import Wmd.Type.SearchTarget (SearchTarget)

data ServerEnv = ServerEnv
  { createUrl :: SearchQuery -> Maybe Text
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
getSearch :: Handler e
getSearch =
  (param "q" >>= toTarget) <|> html searchPage
  where
    toTarget query = do
      query' <- case parseSearchQuery query of
        Right x -> pure x
        Left e -> fail $ Text.unpack e
      createUrl' <- asks createUrl
      url <- case createUrl' query' of
        Just x -> pure x
        Nothing -> fail ""
      redirect $ TextL.fromStrict url

-- | Handles `GET /prefixes`.
getPrefixes :: Handler e
getPrefixes = asks targets >>= html . prefixesPage
