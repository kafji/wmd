module Wmd.Type.Result (
  Result,
) where

import Data.Text (Text)

type Result a = Either Text a
