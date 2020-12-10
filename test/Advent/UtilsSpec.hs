module Advent.UtilsSpec (spec) where

import Advent.Utils (combinations, isBetween, occurrences, readInt, rightToMaybe)
import Data.Foldable (for_)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "occurrences" $ do
    it "finds the number of occurrences in a list" $
      occurrences 'b' "abcdefabc" `shouldBe` 2
    it "is 0 on an empty list" $
      occurrences 42 ([] :: [Int]) `shouldBe` 0

  describe "rightToMaybe" $ do
    it "is 'Just a value' when given a 'Right value'" $
      rightToMaybe (Right 100 :: Either String Int) `shouldBe` Just 100
    it "is 'Nothing' when given a 'Left value'" $
      rightToMaybe (Left "Error" :: Either String Int) `shouldBe` Nothing

  describe "readInt" $
    it "is an int" $
      readInt "123" `shouldBe` 123

  describe "isBetween" $
    context "given a range of 1 and 13" $
      let lower = 1 :: Int
          upper = 13 :: Int
          cases =
            [ (0, False),
              (1, True),
              (7, True),
              (13, True),
              (15, False)
            ]
          test (target, expected) =
            it ("is " ++ show expected ++ " for " ++ show target) $
              isBetween lower upper target `shouldBe` expected
       in for_ cases test

  describe "combinations" $
    it "is the combinations of a list" $ do
      combinations 2 "abcd" `shouldBe` ["ab", "ac", "ad", "bc", "bd", "cd"]
