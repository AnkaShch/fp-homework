{-# LANGUAGE BlockArguments #-}

module TestBlock3.Task1
  ( test
  ) where

import Block3.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test = testSpec "Task1" $ do
  let listTest :: [Maybe [Int]]
      listTest = [Just [1,2,3], Nothing, Just [4,5]]
  let listAns = [1,2,3,4,5]

  describe "maybeConcat" $ do
    it "[Just [1,2,3], Nothing, Just [4,5]] -> [1,2,3,4,5]" $ maybeConcat listTest `shouldBe` listAns
    it "[Noting] -> []" $ maybeConcat [Nothing :: Maybe [Int]] `shouldBe` ([]::[Int])

  let listTestEither :: [Either [Int] [Int]]
      listTestEither = [Left [3], Right [1,2,3], Left [5], Right [4,5]]
  let ansEither :: ([Int], [Int])
      ansEither = ([3, 5], [1,2,3,4,5])

  describe "eitherConcat" $ do
    it "[Left [3], Right [1,2,3], Left [5], Right [4,5]] -> ([3, 5], [1,2,3,4,5])"
      $ eitherConcat listTestEither `shouldBe` ansEither
