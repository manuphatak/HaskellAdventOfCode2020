module Day13.SolutionSpec (spec) where

import Data.Foldable (for_)
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

  describe "busAlignment" $ do
    let cases =
          [ ("7,13,x,x,59,x,31,19", 1068781),
            ("17,x,13,19", 3417),
            ("67,7,59,61", 754018),
            ("67,x,7,59,61", 779210),
            ("67,7,x,59,61", 1261476),
            ("1789,37,47,1889", 1202161486)
          ]

    let test (input, expected) = it ("aligns at " ++ show expected ++ " given a schedule of " ++ input) $ do
          let Right busIds = parseBusIds input
          busAlignment busIds `shouldBe` expected

    for_ cases test
