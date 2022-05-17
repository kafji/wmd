{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Wmd.TargetURLFactory (
  TargetURLFactory (TargetURLFactory),
  createTargetURL,
) where

import Control.Monad.Reader
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Wmd.SearchQuery
import Wmd.Types

type TargetURLProducer = QueryKeywords -> URL

data TargetURLFactory = TargetURLFactory
  { default' :: TargetURLProducer
  , registry :: M.HashMap QueryPrefix TargetURLProducer
  }

createTargetURL :: (MonadReader TargetURLFactory m, MonadFail m) => SearchQuery -> m URL
createTargetURL query = do
  let Query pf kw = query
  gen <- case pf of
    Just pf' -> fromPrefix pf'
    Nothing -> fromDefault
  pure $ gen kw
  where
    fromPrefix pf = do
      tmplts <- asks registry
      case M.lookup pf tmplts of
        Just x -> pure x
        Nothing -> fail $ "missing generator for " <> T.unpack pf
    fromDefault = asks default'
