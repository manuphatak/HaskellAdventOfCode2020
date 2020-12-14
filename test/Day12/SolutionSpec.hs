module Day12.SolutionSpec (spec) where

import Day12.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day12/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day12/input.txt"
    part2 input `shouldBe` "hello santa"
  let exampleInstructions :: [Instruction]
      exampleInstructions = [(F, 10), (N, 3), (F, 7), (R, 90), (F, 11)]
  describe "parseInstructions" $ do
    context "given the input from 'example.txt'" $ do
      it "parses a waiting area" $ do
        input <- readFile "./test/Day12/example.txt"
        parseInstructions input `shouldBe` Right exampleInstructions
