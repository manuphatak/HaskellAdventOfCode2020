module Day10.SolutionSpec (spec) where

import Day10.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day10/input.txt"
    part1 input `shouldBe` "2376"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day10/input.txt"
    part2 input `shouldBe` "hello santa"
  describe "joltageJumps" $ do
    context "given the file example-1.txt" $ do
      let expected = (7, 0, 5)
      it ("has joltage jumps of" ++ show expected) $ do
        input <- readFile "./test/Day10/example-1.txt"

        joltageJumps input `shouldBe` expected
    context "given the file example-2.txt" $ do
      let expected = (22, 0, 10)
      it ("has joltage jumps of" ++ show expected) $ do
        input <- readFile "./test/Day10/example-2.txt"

        joltageJumps input `shouldBe` expected
