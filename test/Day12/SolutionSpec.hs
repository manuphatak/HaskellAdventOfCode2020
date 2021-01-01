{-# LANGUAGE TypeApplications #-}

module Day12.SolutionSpec (spec) where

import Day12.Solution
import Test.Hspec

spec :: Spec
spec = focus . parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day12/input.txt"
    part1 input `shouldBe` "1645"
  it "solves Part 2" $ do
    input <- readFile "./test/Day12/input.txt"
    part2 input `shouldBe` "35292"
  let exampleInstructions :: [Instruction]
      exampleInstructions = [ForwardAction 10, MoveAction North 3, ForwardAction 7, RotateAction [R], ForwardAction 11]
  describe "parseInstructions" $ do
    context "given the input from 'example.txt'" $ do
      it "parses a waiting area" $ do
        input <- readFile "./test/Day12/example.txt"
        parseInstructions input `shouldBe` Right exampleInstructions

  describe "run" $ do
    it "runs through the instructions using the heading as state" $ do
      run @Heading exampleInstructions `shouldBe` ((17, -8), South)
    it "runs through the instructions using the waypoint as state" $ do
      run @Waypoint exampleInstructions `shouldBe` ((214, -72), Waypoint (4, -10))

  describe "manhattanDistance" $ do
    it "finds the distance of a point" $ do
      manhattanDistance (17, -8) `shouldBe` 25

-- describe "runV1" $ do
--   it "runs through the instructions" $ do
--     runV1 exampleInstructions `shouldBe` Point 17 (-8)
-- describe "runV2" $ do
--   it "runs through the instructions" $ do
--     runV2 exampleInstructions `shouldBe` Point 214 (-72)
-- describe "manhattanDistance" $ do
--   it "finds the distance of a point" $ do
--     manhattanDistance (Point 17 (-8)) `shouldBe` 25
