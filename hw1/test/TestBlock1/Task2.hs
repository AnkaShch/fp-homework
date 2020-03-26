{-# LANGUAGE BlockArguments #-}

module TestBlock1.Task2
  ( test
  ) where

import Block1.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Nat number" $ do
    let zero = Z
    let one = S Z
    let two = S (S Z)
    let four = S (S (S (S Z)))

    describe "sum" $ do
      it "0 + 0 = 0" $ zero `sumNat` zero `shouldBe` zero
      it "1 + 1 = 2" $ one `sumNat` one `shouldBe` two
      it "2 + 2 = 4" $ two `sumNat` two `shouldBe` four

    describe "mult" $ do
      it "0 * 0 = 0" $ zero `mulNat` zero `shouldBe` zero
      it "1 * 1 = 1" $ one `mulNat` one `shouldBe` one
      it "2 * 2 = 4" $ two `mulNat` two `shouldBe` four

    describe "sub" $ do
      it "4 - 2 = 2" $ four `subNat` two `shouldBe` two
      it "4 - 0 = 4" $ four `subNat` zero `shouldBe` four
      it "1 - 1 = 4" $ one `subNat` one `shouldBe` zero
      it "1 - 4 = 0" $ one `subNat` four `shouldBe` zero

    describe "toInt" $ do
      it "Z is 0" $ natToInt zero `shouldBe` 0
      it "S(S(S(S(Z)))) is 4" $ natToInt four `shouldBe` 4

    describe "intToNat" $ do
      it "0 is Z" $ intToNat 0 `shouldBe` zero
      it "2 is S(S(Z))" $ intToNat 2 `shouldBe` two

    describe "isEven" $ do
      it "0 is even" $ isEven zero `shouldBe` True
      it "4 is even" $ isEven four `shouldBe` True
      it "1 isn't even" $ isEven one `shouldBe` False

    describe "div" $ do
      it "21 div 4 = 5" $ intToNat 21 `divNat` four `shouldBe` intToNat 5
      it "4 div 2 = 2" $ four `divNat` two `shouldBe` two
      it "4 div 1 = 4" $ four `divNat` one `shouldBe` four
      it "2 div 3 = 0" $ two `divNat` S (S (S Z)) `shouldBe` zero

    describe "mod" $ do
      it "21 mod 4 = 1" $ intToNat 21 `modNat` four `shouldBe` one
      it "4 mod 2 = 0" $ four `modNat` two `shouldBe` zero
      it "4 mod 1 = 0" $ four `modNat` one `shouldBe` zero
      it "2 mod 3 = 2" $ two `modNat` S (S (S Z)) `shouldBe` S (S Z)

    describe "Eq" $ do
      it "4 == 4" $ four == four `shouldBe` True
      it "0 == 0" $ zero == zero `shouldBe` True
      it "1 != 2" $ one == two `shouldBe` False

    describe "Ord" $ do
      it "4 <= 4" $ four <= four `shouldBe` True
      it "0 <= 1" $ zero <= one `shouldBe` True
      it "2 > 1" $ two <= one `shouldBe` False
      it "4 > 2" $ four > two `shouldBe` True
