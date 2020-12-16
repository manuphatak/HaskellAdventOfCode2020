module Day14.SolutionSpec (spec) where

import Data.Foldable (for_)
import qualified Data.IntMap.Strict as IntMap
import Day14.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  fit "solves Part 1" $ do
    input <- readFile "./test/Day14/input.txt"
    part1 input `shouldBe` "12408060320841"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day14/input.txt"
    part2 input `shouldBe` "hello santa"
  let exampleInstructions =
        [ Mask [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just True, Nothing, Nothing, Nothing, Nothing, Just False, Nothing],
          Memory 8 11,
          Memory 7 101,
          Memory 8 0
        ]
  describe "parseInstructions" $ do
    it "parses instructions from text" $ do
      input <- readFile "./test/Day14/example.txt"
      parseInstructions input `shouldBe` Right exampleInstructions

  describe "runProgram" $ do
    let cases :: [(Int, Int)]
        cases = [(7, 101), (8, 64)]
    let test (address, expected) = it ("has a value of " ++ show expected ++ " at address " ++ show address) $ do
          (IntMap.lookup address . stateMemory . runProgram) exampleInstructions `shouldBe` Just expected

    for_ cases test

    it "should sum the remaining values" $ do
      (sum . stateMemory . runProgram) exampleInstructions `shouldBe` 165
