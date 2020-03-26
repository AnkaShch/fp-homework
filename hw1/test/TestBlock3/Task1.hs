{-# LANGUAGE BlockArguments #-}

module TestBlock3.Task1
  ( test
  ) where

import Block3.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test = testSpec "test meybeConcat" $ do
  let listTest :: [Maybe [Int]]
      listTest = [Just [1,2,3], Nothing, Just [4,5]]
  let listAns = [1,2,3,4,5]

  describe "some test" $ do
    it "[Just [1,2,3], Nothing, Just [4,5]] -> [1,2,3,4,5]" $ maybeConcat listTest `shouldBe` listAns
    it "[Noting] -> []" $ maybeConcat [Nothing :: Maybe [Int]] `shouldBe` ([]::[Int])