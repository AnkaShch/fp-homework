import Test.Tasty (defaultMain)

import qualified TestParsers (test)

main :: IO ()
main = do
  tests <- TestParsers.test
  defaultMain tests