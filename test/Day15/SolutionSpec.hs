module Day15.SolutionSpec (spec) where

import Data.Foldable
import Data.Function
import Day15.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day15/input.txt"
    part1 input `shouldBe` "694"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day15/input.txt"
    part2 input `shouldBe` "hello santa"
  describe "memoryGame" $ do
    context "when taking the first 10 results" $ do
      let startingNumbers = [0, 3, 6] :: [Int]
      let expected = [0, 3, 6, 0, 3, 3, 1, 0, 4, 0] :: [Int]
      it ("is " ++ show expected ++ " given starting numbers of " ++ show startingNumbers) $ do
        (memoryGame startingNumbers & take 10) `shouldBe` expected

    context "when looking at the 2020th result" $ do
      let cases :: [([Int], Int)]
          cases =
            [ ([0, 3, 6], 436),
              ([1, 3, 2], 1),
              ([2, 1, 3], 10),
              ([1, 2, 3], 27),
              ([2, 3, 1], 78),
              ([3, 2, 1], 438),
              ([3, 1, 2], 1836)
            ]
      let test (input, expected) = it ("is " ++ show expected ++ " for input " ++ show input) $ do
            (memoryGame input !! (2020 - 1)) `shouldBe` expected

      for_ cases test
