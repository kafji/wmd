{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( app,
  )
where

import Config (getConfig)
import Http.Server qualified as Server (start)

app :: IO ()
app = do
  config <- getConfig
  putStrLn "hello" >> Server.start
