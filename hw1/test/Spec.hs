import Test.Tasty (defaultMain, testGroup)

import qualified TestBlock1.Task1 (test)
import qualified TestBlock1.Task2 (test)
import qualified TestBlock1.Tree (test)
import qualified TestBlock2.Task1 (myTest)
import qualified TestBlock3.Task1 (test)

main :: IO ()
main = do
  testBlock1Task1 <- TestBlock1.Task1.test
  testBlock1Task2 <- TestBlock1.Task2.test
  testBlock1Task3 <- TestBlock1.Tree.test
--  defaultMain $ testGroup "Block1" [testBlock1Task1, testBlock1Task2, testBlock1Task3]
  
  testBlock2Task1 <- TestBlock2.Task1.myTest
--  defaultMain $ testGroup "Blok2" [testBlock2Task1]
  testBlock3Task1 <- TestBlock3.Task1.test
  defaultMain $ testGroup "Block3" [testBlock3Task1]
  