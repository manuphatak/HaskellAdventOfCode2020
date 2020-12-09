module Day09.SolutionSpec (spec) where

import Day09.Solution (part1, part2, xmasCypher)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day09/input.txt"
    part1 input `shouldBe` "167829540"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day09/input.txt"
    part2 input `shouldBe` "hello santa"
  describe "xmasCypher" $ do
    it "finds the first number that does not follow the preamble" $ do
      input <- readFile "./test/Day09/example.txt"
      xmasCypher 5 input `shouldBe` Just 127
