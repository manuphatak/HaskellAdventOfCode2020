module Advent.UtilsSpec (spec) where

import Advent.Utils (occurrences)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "occurrences" $ do
    it "finds the number of occurrences in a list" $
      occurrences 'b' "abcdefabc" `shouldBe` 2
    it "is 0 on an empty list" $
      occurrences 42 ([] :: [Int]) `shouldBe` 0
