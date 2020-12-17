module Day14.SolutionSpec (spec) where

import Data.Foldable (for_)
import qualified Data.IntMap.Strict as IntMap
import Day14.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day14/input.txt"
    part1 input `shouldBe` "12408060320841"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day14/input.txt"
    part2 input `shouldBe` "hello santa"
  let example1Instructions =
        [ SetMask [X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, B True, X, X, X, X, B False, X],
          SetMemory 8 11,
          SetMemory 7 101,
          SetMemory 8 0
        ]
  let example2Instructions =
        [ SetMask [B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, X, B True, B False, B False, B True, X],
          SetMemory 42 100,
          SetMask [B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, B False, X, B False, X, X],
          SetMemory 26 1
        ]
  describe "parseInstructions" $ do
    context "given example-1.txt" $ do
      it "parses instructions from text" $ do
        input <- readFile "./test/Day14/example-1.txt"
        parseInstructions input `shouldBe` Right example1Instructions
    context "given example-2.txt" $ do
      it "parses instructions from text" $ do
        input <- readFile "./test/Day14/example-2.txt"
        parseInstructions input `shouldBe` Right example2Instructions
    context "given example-1.txt" $ do
      it "parses instructions from text" $ do
        input <- readFile "./test/Day14/example-1.txt"
        parseInstructions input `shouldBe` Right example1Instructions

  describe "runProgram" $ do
    context "given example-1.txt" $ do
      let cases :: [(Int, Int)]
          cases = [(7, 101), (8, 64)]
      let test (address, expected) = it ("has a value of " ++ show expected ++ " at address " ++ show address) $ do
            (IntMap.lookup address . stateMemory . runProgram reducerV1) example1Instructions `shouldBe` Just expected

      for_ cases test

      it "should sum the remaining values" $ do
        (sum . stateMemory . runProgram reducerV1) example1Instructions `shouldBe` 165
  describe "runProgramV2" $ do
    context "given example-2.txt" $ do
      it "should sum the remaining values" $ do
        (sum . stateMemory . runProgram reducerV2) example2Instructions `shouldBe` 208
