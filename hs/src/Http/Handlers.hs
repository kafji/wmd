module Http.Handlers (getIndex) where

import Network.Wai (Request, Response)

-- | Handles `GET /`.
getIndex :: Request -> Response
getIndex _ = undefined
