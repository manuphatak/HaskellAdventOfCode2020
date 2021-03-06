module Day11.SolutionSpec (spec) where

import Advent.Utils (occurrences)
import qualified Data.Map.Strict as Map
import Day11.Solution
  ( Point (..),
    Token (..),
    WaitingArea,
    nextSeatRulesFromAdjacentSeats,
    nextSeatRulesFromFirstVisible,
    parseWaitingArea,
    part1,
    part2,
    runSimulation,
  )
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day11/input.txt"
    part1 input `shouldBe` "2243"
  it "solves Part 2" $ do
    input <- readFile "./test/Day11/input.txt"
    part2 input `shouldBe` "2027"
  let parsedWaitingArea :: WaitingArea
      parsedWaitingArea = Map.fromList [(Point 0 0, EmptySeat), (Point 0 1, Floor), (Point 0 2, EmptySeat), (Point 0 3, EmptySeat), (Point 0 4, Floor), (Point 0 5, EmptySeat), (Point 0 6, EmptySeat), (Point 0 7, Floor), (Point 0 8, EmptySeat), (Point 0 9, EmptySeat), (Point 1 0, EmptySeat), (Point 1 1, EmptySeat), (Point 1 2, EmptySeat), (Point 1 3, EmptySeat), (Point 1 4, EmptySeat), (Point 1 5, EmptySeat), (Point 1 6, EmptySeat), (Point 1 7, Floor), (Point 1 8, EmptySeat), (Point 1 9, EmptySeat), (Point 2 0, EmptySeat), (Point 2 1, Floor), (Point 2 2, EmptySeat), (Point 2 3, Floor), (Point 2 4, EmptySeat), (Point 2 5, Floor), (Point 2 6, Floor), (Point 2 7, EmptySeat), (Point 2 8, Floor), (Point 2 9, Floor), (Point 3 0, EmptySeat), (Point 3 1, EmptySeat), (Point 3 2, EmptySeat), (Point 3 3, EmptySeat), (Point 3 4, Floor), (Point 3 5, EmptySeat), (Point 3 6, EmptySeat), (Point 3 7, Floor), (Point 3 8, EmptySeat), (Point 3 9, EmptySeat), (Point 4 0, EmptySeat), (Point 4 1, Floor), (Point 4 2, EmptySeat), (Point 4 3, EmptySeat), (Point 4 4, Floor), (Point 4 5, EmptySeat), (Point 4 6, EmptySeat), (Point 4 7, Floor), (Point 4 8, EmptySeat), (Point 4 9, EmptySeat), (Point 5 0, EmptySeat), (Point 5 1, Floor), (Point 5 2, EmptySeat), (Point 5 3, EmptySeat), (Point 5 4, EmptySeat), (Point 5 5, EmptySeat), (Point 5 6, EmptySeat), (Point 5 7, Floor), (Point 5 8, EmptySeat), (Point 5 9, EmptySeat), (Point 6 0, Floor), (Point 6 1, Floor), (Point 6 2, EmptySeat), (Point 6 3, Floor), (Point 6 4, EmptySeat), (Point 6 5, Floor), (Point 6 6, Floor), (Point 6 7, Floor), (Point 6 8, Floor), (Point 6 9, Floor), (Point 7 0, EmptySeat), (Point 7 1, EmptySeat), (Point 7 2, EmptySeat), (Point 7 3, EmptySeat), (Point 7 4, EmptySeat), (Point 7 5, EmptySeat), (Point 7 6, EmptySeat), (Point 7 7, EmptySeat), (Point 7 8, EmptySeat), (Point 7 9, EmptySeat), (Point 8 0, EmptySeat), (Point 8 1, Floor), (Point 8 2, EmptySeat), (Point 8 3, EmptySeat), (Point 8 4, EmptySeat), (Point 8 5, EmptySeat), (Point 8 6, EmptySeat), (Point 8 7, EmptySeat), (Point 8 8, Floor), (Point 8 9, EmptySeat), (Point 9 0, EmptySeat), (Point 9 1, Floor), (Point 9 2, EmptySeat), (Point 9 3, EmptySeat), (Point 9 4, EmptySeat), (Point 9 5, EmptySeat), (Point 9 6, EmptySeat), (Point 9 7, Floor), (Point 9 8, EmptySeat), (Point 9 9, EmptySeat)]
  describe "parseWaitingArea" $ do
    context "given the input from 'example.txt'" $ do
      it "parses a waiting area" $ do
        input <- readFile "./test/Day11/example.txt"
        parseWaitingArea input `shouldBe` Right parsedWaitingArea
  describe "runSimulation" $ do
    context "given a parseWaitingArea" $ do
      context "given nextSeatRules looking at adjacent seats" $ do
        it "runs the simulation until stabilization" $ do
          (occurrences OccupiedSeat . runSimulation nextSeatRulesFromAdjacentSeats) parsedWaitingArea `shouldBe` (37 :: Int)
      context "given nextSeatRules looking at the first visible seats" $ do
        it "runs the simulation until stabilization" $ do
          (occurrences OccupiedSeat . runSimulation nextSeatRulesFromFirstVisible) parsedWaitingArea `shouldBe` (26 :: Int)
