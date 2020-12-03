module Day03.SolutionSpec (spec) where

import Day03.Solution (part1, part2, slopePath)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day03/input.txt"
    part1 input `shouldBe` "254"
  it "solves Part 2" $ do
    input <- readFile "./test/Day03/input.txt"
    part2 input `shouldBe` "1666768320"
  describe "slopePath" $ do
    context "given the example input and slope" $ do
      it "should follow the input to build a path" $ do
        input <- readFile "./test/Day03/example.txt"
        let expected = ".#.##.####"
        slopePath 3 1 (lines input) `shouldBe` expected
