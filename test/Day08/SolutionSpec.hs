module Day08.SolutionSpec (spec) where

import Advent.Utils (fromLeft')
import Data.Foldable (for_)
import Day08.Solution
  ( Instruction (..),
    Operation (..),
    Program (..),
    Sign (..),
    fixProgram,
    fixedInstructions,
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
  it "solves Part 2" $ do
    input <- readFile "./test/Day08/input.txt"
    part2 input `shouldBe` "1033"
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
        (programAcc . fromLeft' . runProgram initialState) parsedExample `shouldBe` 5
  describe "fixProgram" $ do
    context "given instructions from example.txt" $ do
      it "has an accumulator of 5" $ do
        programAcc <$> fixProgram initialState parsedExample `shouldBe` Right 8

  describe "fixInstructions" $ do
    context "given instructions from example.txt" $ do
      let cases =
            [ (0, Instruction Jump Plus 0),
              (1, Instruction Accumulator Plus 1),
              (2, Instruction NoOperation Plus 4),
              (3, Instruction Accumulator Plus 3),
              (4, Instruction NoOperation Minus 3),
              (5, Instruction Accumulator Minus 99),
              (6, Instruction Accumulator Plus 1),
              (7, Instruction NoOperation Minus 4),
              (8, Instruction Accumulator Plus 6)
            ]
          test (index, expected) = it "fixes the instructions" $ do
            fixedInstructions parsedExample !! index !! index `shouldBe` expected
       in for_ cases test
