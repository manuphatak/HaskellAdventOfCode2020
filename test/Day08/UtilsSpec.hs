module Day08.UtilsSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import Day08.Utils (asIntMap, fromLeftOrError, fromRightOrError)
import Test.Hspec

type TestType = Either Int Int

spec :: Spec
spec = parallel $ do
  describe "asIntMap" $ do
    it "converts a list to an IntMap" $ do
      asIntMap ["day 1", "day 2", "day 3", "day 4"] `shouldBe` IntMap.fromList [(0, "day 1"), (1, "day 2"), (2, "day 3"), (3, "day 4")]
  describe "fromLeftOrError" $ do
    context "given a Left Value" $ do
      it "is the Left value" $ do
        fromLeftOrError (Left 42 :: TestType) `shouldBe` 42
    context "given a Right Value" $ do
      it "is throws an exceptions" $ do
        evaluate (fromLeftOrError (Right 42 :: TestType)) `shouldThrow` anyException
  describe "fromRightOrError" $ do
    context "given a Right Value" $ do
      it "is the Right value" $ do
        fromRightOrError (Right 53 :: TestType) `shouldBe` 53
    context "given a Left Value" $ do
      it "is throws an exceptions" $ do
        evaluate (fromRightOrError (Left 21 :: TestType)) `shouldThrow` anyException
