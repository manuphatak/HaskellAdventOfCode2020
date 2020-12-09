module Day09.UtilsSpec (spec) where

import Day09.Utils (parseNumbers, rollingChunks)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "rollingChunks" $ do
    it "iterates through a list for case 1" $
      rollingChunks 2 "abcd" `shouldBe` ["ab", "bc", "cd"]
    it "iterates through a list for case 2" $
      rollingChunks 3 "abcdef" `shouldBe` ["abc", "bcd", "cde", "def"]
  describe "parseNumbers" $ do
    it "parses a string into numbers" $
      parseNumbers "1\n3\n5\n7\n11\n13\n17\n" `shouldBe` [1, 3, 5, 7, 11, 13, 17]
