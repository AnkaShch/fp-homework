{-# LANGUAGE BlockArguments #-}
module TestBlock1.Task1
  (
    test
  ) where

import Block1.Task1
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec
  (
    describe
    , it
    , shouldBe
    , shouldSatisfy
    , shouldNotSatisfy
    , testSpec
  )

test :: IO TestTree
test = testSpec "Days of Week" $ do
  describe "next Day" $ do
    it "Mond -> Tues" $ nextDay Monday `shouldBe` Tuesday
    it "Fri -> Sat" $ nextDay Friday `shouldBe` Saturday
    it "Sun -> Mond" $ nextDay Sunday `shouldBe` Monday
    
  describe "afterDays" $ do
    it "Mond is Mond after 14 days" $ Monday `afterDays` 14 `shouldBe` Monday
    it "Fri is Mond after 3 days" $ Friday `afterDays` 3 `shouldBe` Monday
    it "Sat is _ after 10 days" $ Saturday `afterDays` 10 `shouldBe` Tuesday