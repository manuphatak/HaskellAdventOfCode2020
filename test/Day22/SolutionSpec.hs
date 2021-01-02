module Day22.SolutionSpec (spec) where

import Day22.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day22/input.txt"
    part1 input `shouldBe` "hello_santa"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day22/input.txt"
    part2 input `shouldBe` "hello_santa"
