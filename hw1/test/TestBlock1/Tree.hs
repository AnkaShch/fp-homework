{-# LANGUAGE DataKinds #-}

module TestBlock1.Tree
  ( test
  ) where

import Block1.Tree
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Tree" $ do
    let emptyTree = Leaf
    let tree1 = Node Leaf (5 :: Int) [5, 5, 5] Leaf
    let tree2 = Node (Node Leaf (4 :: Int) [] Leaf) 5 [5, 5, 5] Leaf
    let tree3 = Node (Node Leaf (2 :: Int) [] (Node Leaf 4 [4] Leaf)) 5 [5, 5, 5] Leaf
    let tree4 = Node (Node Leaf 2 [] (Node Leaf 4 [4] Leaf)) 5 [5, 5, 5] (Node Leaf (8 :: Int) [] Leaf)
    let tree5 =
          Node
            (Node Leaf 2 [] (Node Leaf 4 [4] Leaf))
            5
            [5, 5, 5]
            (Node Leaf 8 [] (Node Leaf (10 :: Int) [10, 10, 10] Leaf))

    describe "isEmptyTree" $ do
      it "empty tree" $ isEmptyTree emptyTree `shouldBe` True
      it "not empty tree" $ isEmptyTree tree4 `shouldBe` False

    describe "find Elements" $ do
      it "5 in tree1" $ findElem 5 tree1 `shouldBe` Just 5
      it "3 in tree5" $ findElem (3 :: Int) tree5 `shouldBe` Nothing
      it "1 in empty tree" $ findElem (1 :: Int) emptyTree `shouldBe` Nothing
      it "8 in tree4" $ findElem 8 tree4 `shouldBe` Just 8

    describe "size of tree" $ do
      it "emptyTree size = 0" $ sizeTree emptyTree `shouldBe` 0
      it "tree5 size = 12" $ sizeTree tree5 `shouldBe` 12

    describe "insert elem" $ do
      it "insert 4 in tree1 -> tree2" $ insertElem 4 tree1 `shouldBe` tree2
      it "insert 8 in tree3 -> tree4" $ insertElem 8 tree3 `shouldBe` tree4
      it "insert 1 in empty tree -> Node Leaf 1 [] Leaf" $ insertElem (1 :: Int) emptyTree `shouldBe` Node Leaf 1 [] Leaf

    describe "tree from list" $ do
      it "[5, 5, 4, 5, 5]" $ fromList [5, 5, 4, 5, 5] `shouldBe` tree2
      it "[5, 4, 4, 2, 5, 5, 5]" $ fromList [5, 4, 4, 2, 5, 5, 5] `shouldBe` tree3

    describe "remove elem" $ do
      it "remove 4 in tree2 -> tree1" $ removeElem 4 tree2 `shouldBe` tree1
      it "remove 8 in tree4 -> tree3" $ removeElem 8 tree4 `shouldBe` tree3
      it "remove 1 in Node Leaf 1 [] Leaf -> Leaf" $ removeElem (1 ::Int) (Node Leaf 1 [] Leaf) `shouldBe` emptyTree
