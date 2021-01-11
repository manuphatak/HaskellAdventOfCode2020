module Day24.SolutionSpec (spec) where

import Day24.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day24/input.txt"
    part1 input `shouldBe` "hello_santa"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day24/input.txt"
    part2 input `shouldBe` "hello_santa"
