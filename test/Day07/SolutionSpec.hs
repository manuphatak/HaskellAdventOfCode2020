module Day07.SolutionSpec (spec) where

import Day07.Solution (part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day07/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day07/input.txt"
    part2 input `shouldBe` "hello santa"
