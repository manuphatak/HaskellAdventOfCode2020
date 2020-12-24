module Day17.SolutionSpec (spec) where

import Advent.Utils (occurrences)
import qualified Data.Map.Strict as Map
import Day17.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day17/input.txt"
    part1 input `shouldBe` "215"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day17/input.txt"
    part2 input `shouldBe` "hello_santa"
  let examplePocketDimension =
        Map.fromList
          [ ((0, 0, 0), Inactive),
            ((0, 1, 0), Inactive),
            ((0, 2, 0), Active),
            ((1, 0, 0), Active),
            ((1, 1, 0), Inactive),
            ((1, 2, 0), Active),
            ((2, 0, 0), Inactive),
            ((2, 1, 0), Active),
            ((2, 2, 0), Active)
          ]

  describe "parsePocketDimension" $ do
    it "parses input" $ do
      input <- readFile "./test/Day17/example.txt"
      parsePocketDimension input `shouldBe` Right examplePocketDimension

  describe "executeCycles" $ do
    let cycles = 6 :: Int
    let expected = 112 :: Int
    context ("when running " ++ show cycles ++ " cycles") $ do
      it ("has " ++ show expected ++ " active cubes") $ do
        (occurrences Active . executeCycles cycles) examplePocketDimension `shouldBe` 112
