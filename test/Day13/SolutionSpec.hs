module Day13.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day13.Solution
  ( busAlignment,
    crt,
    earliestBus,
    parseBusIds,
    parseSchedule,
    part1,
    part2,
  )
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day13/input.txt"
    part1 input `shouldBe` "119"
  it "solves Part 2" $ do
    input <- readFile "./test/Day13/input.txt"
    part2 input `shouldBe` "1106724616194525"

  let exampleSchedule = (939, [Just 7, Just 13, Nothing, Nothing, Just 59, Nothing, Just 31, Just 19])
  describe "parseSchedule" $ do
    it "is the schedule" $ do
      input <- readFile "./test/Day13/example.txt"
      parseSchedule input `shouldBe` Right exampleSchedule

  describe "earliestBus" $ do
    it "is" $ do
      earliestBus exampleSchedule `shouldBe` (5, 59)

  describe "busAlignment" $ do
    let cases :: [(String, Integer)]
        cases =
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

  describe "crt" $ do
    let cases :: [([(Integer, Integer)], (Integer, Integer), Integer)]
        cases =
          [ ([(2, 7), (0, 3), (1, 5)], (51, 105), 54),
            ([(2, 3), (3, 4), (1, 5)], (11, 60), 49),
            ([(0, 7), (1, 13), (4, 59), (6, 31), (7, 19)], (2093560, 3162341), 1068781),
            ([(0, 17), (2, 13), (3, 19)], (782, 4199), 3417),
            ([(0, 67), (1, 7), (2, 59), (3, 61)], (933913, 1687931), 754018),
            ([(0, 67), (2, 7), (3, 59), (4, 61)], (908721, 1687931), 779210),
            ([(0, 67), (1, 7), (3, 59), (4, 61)], (426455, 1687931), 1261476),
            ([(0, 1789), (1, 37), (2, 47), (3, 1889)], (4674651633, 5876813119), 1202161486),
            ([(0, 23), (17, 37), (23, 863), (35, 19), (36, 13), (40, 17), (52, 29), (54, 571), (95, 41)], (986925922963328, 2093650539157853), 1106724616194525)
          ]
    let test (input, expected, expected') = it ("is " ++ show expected ++ " given " ++ show input) $ do
          let output@(remainder, modulus) = crt input
          output `shouldBe` expected
          modulus - remainder `shouldBe` expected'

    for_ cases test
