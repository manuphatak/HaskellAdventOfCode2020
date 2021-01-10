module Day17.SolutionSpec (spec) where

import Advent.Utils (occurrences)
import qualified Data.Map.Strict as Map
import Day17.Solution
  ( CubeState (..),
    Point3D (..),
    Point4D (..),
    executeCycles,
    parsePocketDimension,
    part1,
    part2,
  )
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day17/input.txt"
    part1 input `shouldBe` "215"
  it "solves Part 2" $ do
    input <- readFile "./test/Day17/input.txt"
    part2 input `shouldBe` "1728"
  let examplePocketDimension3D =
        Map.fromList
          [ (Point3D (0, 0, 0), Inactive),
            (Point3D (0, 1, 0), Inactive),
            (Point3D (0, 2, 0), Active),
            (Point3D (1, 0, 0), Active),
            (Point3D (1, 1, 0), Inactive),
            (Point3D (1, 2, 0), Active),
            (Point3D (2, 0, 0), Inactive),
            (Point3D (2, 1, 0), Active),
            (Point3D (2, 2, 0), Active)
          ]
  let examplePocketDimension4D =
        Map.fromList
          [ (Point4D (0, 0, 0, 0), Inactive),
            (Point4D (0, 1, 0, 0), Inactive),
            (Point4D (0, 2, 0, 0), Active),
            (Point4D (1, 0, 0, 0), Active),
            (Point4D (1, 1, 0, 0), Inactive),
            (Point4D (1, 2, 0, 0), Active),
            (Point4D (2, 0, 0, 0), Inactive),
            (Point4D (2, 1, 0, 0), Active),
            (Point4D (2, 2, 0, 0), Active)
          ]

  describe "parsePocketDimension" $ do
    context "parsing 3D pockets" $ do
      it "parses input" $ do
        input <- readFile "./test/Day17/example.txt"
        parsePocketDimension input `shouldBe` Right examplePocketDimension3D
    context "parsing 4D pockets" $ do
      it "parses input" $ do
        input <- readFile "./test/Day17/example.txt"
        parsePocketDimension input `shouldBe` Right examplePocketDimension4D

  describe "executeCycles" $ do
    context "given 3D pockets" $ do
      let cycles = 6 :: Int
      let expected = 112 :: Int
      context ("when running " ++ show cycles ++ " cycles") $ do
        it ("has " ++ show expected ++ " active cubes") $ do
          (occurrences Active . executeCycles cycles) examplePocketDimension3D `shouldBe` expected

    context "given 4D pockets" $ do
      let cycles = 6 :: Int
      let expected = 848 :: Int
      context ("when running " ++ show cycles ++ " cycles") $ do
        it ("has " ++ show expected ++ " active cubes") $ do
          (occurrences Active . executeCycles cycles) examplePocketDimension4D `shouldBe` expected
