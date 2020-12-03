module Day03.SolutionSpec (spec) where

import Day03.Solution (followSlope, part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day03/input.txt"
    part1 input `shouldBe` "254"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day03/input.txt"
    part2 input `shouldBe` "hello santa"
  describe "followSlope" $
    context "given the example input" $ do
      it "should count the number of trees" $ do
        input <- readFile "./test/Day03/example.txt"

        followSlope 3 1 input `shouldBe` "7"