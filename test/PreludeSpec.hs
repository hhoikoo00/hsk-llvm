module PreludeSpec (spec) where

import           Control.Exception     (evaluate)
import           Test.Hspec            (Spec, anyException, describe, it,
                                        shouldBe, shouldReturn, shouldSatisfy,
                                        shouldThrow)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "Prelude" $ do
    describe "head" $ do
      it "returns the first element of a list" $ do
        head [23..] `shouldBe` (23 :: Int)

      prop "returns the first element of an *arbitrary* list" $
        \x xs -> head (x:xs) == (x :: Int)

      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException

    describe "read" $ do
      it "can parse integers" $ do
        read "10" `shouldBe` (10 :: Int)

      it "can parse floating-point numbers" $ do
        read "2.5" `shouldBe` (2.5 :: Float)

      it "shows how IO action can be tested" $ do
        return (read "2.5") >>= (`shouldBe` (2.5 :: Float))

      it "shows how IO action can be tested better" $ do
        return (read "2.5") `shouldReturn` (2.5 :: Float)

    describe "null" $ do
      it "should return False for empty list" $ do
        ([] :: [Int]) `shouldSatisfy` null

      it "should return True for non-empty list" $ do
        [1..] `shouldSatisfy` (not . null)
