module Advent.UtilsSpec (spec) where

import Advent.Utils (occurrences, rightToMaybe)
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
