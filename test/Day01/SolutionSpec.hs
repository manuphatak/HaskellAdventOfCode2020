module Day01.SolutionSpec (spec) where

import Day01.Solution (goalSeek, part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day01/input.txt"
    part1 input `shouldBe` "299299"
  it "solves Part 2" $ do
    input <- readFile "./test/Day01/input.txt"
    part2 input `shouldBe` "287730716"
  describe "goalSeek" $ do
    context "given a list of numbers" $
      let needle = 2020
          haystack = [1721, 979, 366, 299, 675, 1456]
       in do
            it "finds the product of the 2 numbers that add up to a target" $
              goalSeek 2 needle haystack `shouldBe` Just (1721 * 299)

            it "finds the product of the 3 numbers that add up to a target" $
              goalSeek 3 needle haystack `shouldBe` Just (979 * 366 * 675)