import Test.Tasty
import Test.Wmd.ConfigTest
import Test.Wmd.PercentEncoding
import Test.Wmd.SearchQuery

main :: IO ()
main = defaultMain tests

tests =
  testGroup
    "Tests"
    [ configTests
    , searchQueryTests
    , percentEncodingTests
    ]
