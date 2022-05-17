{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wmd.HTTPServer (
  ServerEnv (ServerEnv),
  server,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Reader (
  MonadIO,
  MonadReader,
  ReaderT (runReaderT),
  asks,
 )
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (
  ActionT,
  ScottyError,
  ScottyT,
  get,
  html,
  middleware,
  param,
  redirect,
 )
import Wmd.HTML (homePage, searchPage)
import Wmd.SearchQuery (parseSearchQuery)
import Wmd.TargetURLFactory (TargetURLFactory, createTargetURL)

type Server m = ScottyT LazyText.Text m ()

type Handler e m = ActionT e m ()

data ServerEnv = ServerEnv
  { targetUrlGenerator :: TargetURLFactory
  }

server :: (MonadIO m, MonadReader ServerEnv m) => Server m
server = do
  middleware logStdout
  mconcat
    [ get "/" getIndex
    , get "/search" getSearch
    ]

-- | Handles `GET /`.
getIndex :: (MonadIO m, ScottyError e) => Handler e m
getIndex = html $ LazyText.fromStrict homePage

-- | Handles `GET /search`.
getSearch :: forall m e. (MonadIO m, MonadReader ServerEnv m, ScottyError e) => Handler e m
getSearch =
  (param "q" >>= toTarget) <|> html (LazyText.fromStrict searchPage)
  where
    toTarget query = do
      query' <- case parseSearchQuery query of
        Right x -> pure x
        Left e -> fail $ Text.unpack e
      gens <- asks targetUrlGenerator
      url <- case (runReaderT (createTargetURL query') gens) of
        Just x -> pure x
        Nothing -> fail ""
      redirect $ LazyText.fromStrict url
