module Day03.SolutionSpec (spec) where

import Day03.Solution (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day03/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day03/input.txt"
    part2 input `shouldBe` "hello santa"
