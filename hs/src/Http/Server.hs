module Http.Server (start) where

import Http.Handlers (getIndex)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

start :: IO ()
start = run 3000 app

app :: Application
app req han = (han . getIndex) req
