import           Test.Tasty       (defaultMain, testGroup)

import qualified TestBlock1.Task1 (test)
import qualified TestBlock1.Task2 (test)
import qualified TestBlock1.Tree  (test)

main :: IO ()
main = do
  testBlock1Task1 <- TestBlock1.Task1.test
  testBlock1Task2 <- TestBlock1.Task2.test
  testBlock1Task3 <- TestBlock1.Tree.test
  defaultMain $ testGroup "Block1" [testBlock1Task1, testBlock1Task2, testBlock1Task3]
