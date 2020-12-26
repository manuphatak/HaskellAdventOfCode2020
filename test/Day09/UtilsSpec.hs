module Day09.UtilsSpec (spec) where

import Day09.Utils (rollingChunks)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "rollingChunks" $ do
    it "iterates through a list for case 1" $
      rollingChunks 2 "abcd" `shouldBe` ["ab", "bc", "cd"]
    it "iterates through a list for case 2" $
      rollingChunks 3 "abcdef" `shouldBe` ["abc", "bcd", "cde", "def"]
