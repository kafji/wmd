module Wmd.Cli (
  Flags (..),
  getFlags,
) where

import Data.Word (Word16)
import Options.Applicative

type Port = Word16

-- | CLI flags.
data Flags = Flags
  { port :: Port
  }
  deriving (Show, Eq)

-- | Reads CLI flags using @optparse@.
getFlags :: IO Flags
getFlags = execParser $ info (argsParser <**> helper) (fullDesc)

argsParser :: Parser Flags
argsParser = do
  Flags <$> readPort

readPort :: Parser Port
readPort =
  option auto $
    mconcat
      [ long "port"
      , metavar "<port>"
      , help "Set server port"
      ]
