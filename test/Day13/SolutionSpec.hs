module Day13.SolutionSpec (spec) where

import Day13.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day13/input.txt"
    part1 input `shouldBe` "119"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day13/input.txt"
    part2 input `shouldBe` "hello santa"

  let exampleSchedule = (939, [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19])
  describe "parseSchedule" $ do
    it "is the schedule" $ do
      input <- readFile "./test/Day13/example.txt"
      parseSchedule input `shouldBe` Right exampleSchedule

  describe "earliestBus" $ do
    it "is" $ do
      earliestBus exampleSchedule `shouldBe` (5, 59)
