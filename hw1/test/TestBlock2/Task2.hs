{-# LANGUAGE BlockArguments #-}

module TestBlock2.Task2
  ( test
  ) where

import Block2.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test = testSpec "Task2" $ do
  describe "test splitOn" $ do
    it "splitOn '/' 'path/to/file' -> ['path', 'to', 'file']" $ splitOn '/' "path/to/file" `shouldBe` ("path":|["to", "file"])
    it "splitOn '/' 'path' -> ['path']" $ splitOn '/' "path" `shouldBe` ("path":|[])