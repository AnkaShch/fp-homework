{-# LANGUAGE BlockArguments #-}

module TestBlock1.Task1
  ( test
  ) where

import           Block1.Task1
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Days of Week" $ do
    describe "next Day" $ do
      it "Mond -> Tues" $ nextDay Monday `shouldBe` Tuesday
      it "Fri -> Sat" $ nextDay Friday `shouldBe` Saturday
      it "Sun -> Mond" $ nextDay Sunday `shouldBe` Monday
    
    describe "afterDays" $ do
      it "Mond is Mond after 14 days" $ Monday `afterDays` 14 `shouldBe` Monday
      it "Fri is Mond after 3 days" $ Friday `afterDays` 3 `shouldBe` Monday
      it "Sat is Tues after 10 days" $ Saturday `afterDays` 10 `shouldBe` Tuesday
    
    describe "isWeekend" $ do
      it "Sun is weekend" $ isWeekend Sunday `shouldBe` True
      it "Mond isn't weekend" $ isWeekend Monday `shouldBe` False
    
    describe "days to Friday" $ do
      it "from Fri to Fri 0 days" $ daysToParty Friday `shouldBe` 0
      it "from Sat to Fri 6 days" $ daysToParty Saturday `shouldBe` 6
      it "from Tues to Fri 3 days" $ daysToParty Tuesday `shouldBe` 3
