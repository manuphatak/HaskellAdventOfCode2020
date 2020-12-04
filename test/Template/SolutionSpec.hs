module Template.SolutionSpec (spec) where

import Template.Solution (part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Template/input.txt"
    part1 input `shouldBe` "hello santa"
  it "solves Part 2" $ do
    input <- readFile "./test/Template/input.txt"
    part2 input `shouldBe` "hello santa"
