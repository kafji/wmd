module Wmd.App where

import Data.Text (Text)
import Wmd.Config (Config, config)
import Wmd.TargetURLMaker (TargetURLMaker)

type Prefix = Text

type Keywords = Text

data App = App
  { urlGenerators :: TargetURLMaker
  }

app :: MonadFail m => Config -> m App
app cfg = do
  undefined

run :: IO ()
run = config "../wmd.toml" >>= pure . maybe undefined id . app >> undefined
