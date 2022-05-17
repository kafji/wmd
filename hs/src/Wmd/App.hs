module Wmd.App where

import Data.Text (Text)
import Wmd.Config (Config, config)
import Wmd.TargetURLFactory (TargetURLFactory)

type Prefix = Text

type Keywords = Text

data App = App
  { urlGenerators :: TargetURLFactory
  }

app :: MonadFail m => Config -> m App
app cfg = do
  undefined

run :: IO ()
run = config "../wmd.toml" >>= pure . maybe undefined id . app >> undefined
