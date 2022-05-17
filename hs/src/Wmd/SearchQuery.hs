{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Wmd.SearchQuery where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Wmd.Types

data SearchQuery = Query
  { prefix :: (Maybe QueryPrefix)
  , keywords :: QueryKeywords
  }
  deriving (Show, Eq)

type Parser m = MonadParsec Void Text m

separatorParser :: Parser m => m ()
separatorParser = char ' ' >> pure ()

-- >>> parseMaybe prefixParser "hello "
-- Just "hello"
--
-- >>> parseMaybe prefixParser "hello"
-- Nothing
prefixParser :: Parser m => m QueryPrefix
prefixParser = takeWhile1P Nothing (/= ' ') <* separatorParser

-- >>> parseMaybe keywordsParser "world"
-- Just "world"
--
-- >>> parseMaybe keywordsParser " world"
-- Just " world"
keywordsParser :: Parser m => m QueryKeywords
keywordsParser = do
  takeWhile1P Nothing (const True)

-- >>> parseMaybe completeQueryParser "hello world"
-- Just (Query (Just "hello") "world")
--
-- >>> parseMaybe completeQueryParser "hello"
-- Nothing
completeQueryParser :: Parser m => m SearchQuery
completeQueryParser = do
  p <- prefixParser
  k <- keywordsParser
  pure $ Query (Just p) k

-- >>> parseMaybe partialQueryParser "hello world"
-- Just (Query Nothing "hello world")
--
-- >>> parseMaybe partialQueryParser "hello"
-- Just (Query Nothing "hello")
partialQueryParser :: Parser m => m SearchQuery
partialQueryParser = do
  k <- keywordsParser
  pure $ Query Nothing k

-- >>> parseMaybe queryParser "hello   world"
-- Just (Query (Just "hello") "  world")
--
-- >>> parseMaybe queryParser "hello"
-- Just (Query Nothing "hello")
queryParser :: Parser m => m SearchQuery
queryParser = try completeQueryParser <|> partialQueryParser

-- >>> parseSearchQuery "hello   world"
-- Right (Query (Just "hello") "  world")
--
-- >>> parseSearchQuery "hello"
-- Right (Query Nothing "hello")
parseSearchQuery :: Text -> Either Text SearchQuery
parseSearchQuery s = maybe (Left "error") Right $ parseMaybe queryParser s
