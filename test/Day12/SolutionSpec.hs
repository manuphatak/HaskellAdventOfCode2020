module Day12.SolutionSpec (spec) where

import Day12.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day12/input.txt"
    part1 input `shouldBe` "1645"
  it "solves Part 2" $ do
    input <- readFile "./test/Day12/input.txt"
    part2 input `shouldBe` "35292"
  let exampleInstructions :: [Instruction]
      exampleInstructions = [(F, 10), (N, 3), (F, 7), (R, 90), (F, 11)]
  describe "parseInstructions" $ do
    context "given the input from 'example.txt'" $ do
      it "parses a waiting area" $ do
        input <- readFile "./test/Day12/example.txt"
        parseInstructions input `shouldBe` Right exampleInstructions
  describe "runV1" $ do
    it "runs through the instructions" $ do
      runV1 exampleInstructions `shouldBe` Point 17 (-8)
  describe "runV2" $ do
    it "runs through the instructions" $ do
      runV2 exampleInstructions `shouldBe` Point 214 (-72)
  describe "manhattanDistance" $ do
    it "finds the distance of a point" $ do
      manhattanDistance (Point 17 (-8)) `shouldBe` 25
