{-# LANGUAGE BlockArguments #-}

module TestBlock3.Task2
  ( test
  ) where

import Block3.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Task2" $ do
    let arr1 :: NonEmpty Int
        arr1 = 1 :| [1, 1]
    let arr2 :: NonEmpty Int
        arr2 = 2 :| [2, 2]
    let arr3 :: NonEmpty Int
        arr3 = 1 :| [1, 1, 2, 2, 2]
    describe "test <>" $ do
      it "[1,1,1] <> [2,2,2] -> [1,1,1,2,2,2]" $ arr1 <> arr2 `shouldBe` arr3
