module Day08.SolutionSpec (spec) where

import Day08.Solution
  ( Instruction (..),
    Operation (..),
    Program (..),
    Sign (..),
    initialState,
    parseInstructions,
    part1,
    part2,
    runProgram,
  )
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day08/input.txt"
    part1 input `shouldBe` "1317"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day08/input.txt"
    part2 input `shouldBe` "hello santa"
  let parsedExample =
        [ Instruction NoOperation Plus 0,
          Instruction Accumulator Plus 1,
          Instruction Jump Plus 4,
          Instruction Accumulator Plus 3,
          Instruction Jump Minus 3,
          Instruction Accumulator Minus 99,
          Instruction Accumulator Plus 1,
          Instruction Jump Minus 4,
          Instruction Accumulator Plus 6
        ]
  describe "parseInstructions" $ do
    it "parses the example into instructions" $ do
      input <- readFile "./test/Day08/example.txt"
      parseInstructions input `shouldBe` Right parsedExample

  describe "runProgram" $ do
    context "given instructions from example.txt" $ do
      it "has an accumulator of 5" $ do
        programAcc (runProgram initialState parsedExample) `shouldBe` 5
