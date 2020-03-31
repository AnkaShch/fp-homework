{-# LANGUAGE BlockArguments #-}

module TestParsers
  ( test
  ) where

import Block6.Parsers
import Data.Char
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Parsers" $ do
    describe "test ok" $ do
      it "asdf" $ runParser ok "asdf" `shouldBe` Just ((), "asdf")
      it "empty test" $ runParser ok "" `shouldBe` Just ((), "")

    describe "test eof" $ do
      it "eof" $ runParser eof "" `shouldBe` Just ((), "")
      it "asdf" $ runParser eof "asdf" `shouldBe` Nothing

    describe "test satisfy" $ do
      it "isUpper ASDF" $ runParser (satisfy isUpper) "ASDF" `shouldBe` Just ('A',"SDF")
      it "isUpper asdf" $ runParser (satisfy isUpper) "asdf" `shouldBe` Nothing
      it "isDigit 1asd" $ runParser (satisfy isDigit) "1asd" `shouldBe` Just ('1', "asd")

    describe "test element" $ do
      it "a asdf" $ runParser (element 'a') "asdf" `shouldBe` Just('a', "sdf")
      it "a zxcv" $ runParser (element 'a') "zxcv" `shouldBe` Nothing

    describe "test stream" $ do
      it "asd asdfg" $ runParser (stream "asd") "asdfg" `shouldBe` Just("asd", "fg")
      it "asd zxcvbn" $ runParser (stream "asd") "zxcvbn" `shouldBe` Nothing
      it "asd asd" $ runParser (stream "asd") "asd" `shouldBe` Just("asd", "")

    describe "test CBS" $ do
      it "(())" $ runParser parseCBS "(())" `shouldBe` Just((), "")
      it "()()" $ runParser parseCBS "()()" `shouldBe` Just((), "")
      it "(()" $ runParser parseCBS "(()" `shouldBe` Nothing
      it "))" $ runParser parseCBS "))" `shouldBe` Nothing
    
    describe "test parseInt" $ do
      it "13" $ runParser parseInt "13" `shouldBe` Just(13, "")
      it "-13asdfg" $ runParser parseInt "-13asdfg" `shouldBe` Just(-13, "asdfg")
      it "asdf" $ runParser parseInt "asdf" `shouldBe` Nothing
      it "-" $ runParser parseInt "-" `shouldBe` Nothing
      it "+1-1" $ runParser parseInt "+1-1" `shouldBe` Just(1, "-1")
