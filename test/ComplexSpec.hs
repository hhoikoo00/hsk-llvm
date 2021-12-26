module ComplexSpec (spec) where

import           Control.Exception     (evaluate)
import           Test.Hspec            (Spec, after_, before_, describe, it,
                                        pending, pendingWith, shouldBe)
import           Test.Hspec.QuickCheck (prop)

beforeAction :: IO ()
beforeAction = putStrLn "\tAction before..."

afterAction :: IO ()
afterAction = putStrLn "\t...Action after"

spec :: Spec
spec = after_ afterAction $ before_ beforeAction $ do
  describe "/login" $ do
    it "should always pass" $ do
      putStrLn "\tPASS"
      3 `shouldBe` 3

    it "should use correct status codes" $ do
      pending

    it "should require basic authentication" $ do
      pendingWith "need to fix base64 first"
