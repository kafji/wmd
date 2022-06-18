module Wmd.Test.HttpServerTest where

import Data.Vector qualified as Vector
import Network.Wai.Test (SResponse (simpleBody), defaultRequest, request, setPath, withSession)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden.Easy (declModulePath, expect)
import Wmd.HttpServer (ServerEnv (ServerEnv), getServer)
import Wmd.Type.SearchTarget (SearchTarget (SearchTarget))

$(declModulePath)

httpServerTests :: TestTree
httpServerTests =
  testGroup
    "http server"
    [ getPrefixesTest
    ]

getPrefixesTest = $(expect) "GET /prefixes" do
  server <- getServer (ServerEnv (const Nothing) targets)
  body <- withSession server $ simpleBody <$> request (setPath defaultRequest "/prefixes")
  pure body

targets =
  Vector.fromList
    [ SearchTarget "tst" "Test" "https://example.com?q={keywords}"
    ]
