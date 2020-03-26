{-# LANGUAGE BlockArguments, DataKinds #-}

module TestBlock2.Task1
  ( myTest
  ) where

import Block1.Tree

import Data.Foldable (toList)
import Data.List (sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog (hedgehog)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, testSpec)

myTest :: IO TestTree
myTest = testSpec "Tree" $ do
  let genArr :: PropertyT IO [Int]
      genArr = forAll $ Gen.list (Range.linear 5 10) (Gen.int $ Range.linear 1 10)

  describe "test Foldable" $ do
    it "property" $ hedgehog $ do
      arr <- genArr
      (toList . Block1.Tree.fromList) arr === sort arr

