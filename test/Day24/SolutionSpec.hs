module Day24.SolutionSpec (spec) where

import Advent.Utils (occurrences)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Day24.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day24/input.txt"
    part1 input `shouldBe` "275"

  it "solves Part 2" $ do
    pendingWith "needs performance boost"
    input <- readFile "./test/Day24/input.txt"
    part2 input `shouldBe` "3537"

  let exampleTilePaths =
        [ [SE, SE, NW, NE, NE, NE, W, SE, E, SW, W, SW, SW, W, NE, NE, W, SE, W, SW],
          [NE, E, E, NE, SE, NW, NW, W, SW, NE, NE, W, NW, W, SE, W, NE, NW, SE, SW, E, SW],
          [SE, SW, NE, SW, SW, SE, NW, W, NW, SE],
          [NW, NW, NE, SE, E, SW, SW, NE, NE, W, NE, SW, W, NE, W, SE, SW, NE, SE, E, NE],
          [SW, W, E, SW, NE, SW, NE, NW, SE, W, NW, NE, NE, SE, E, NW],
          [E, E, SE, NW, SE, SW, SW, NE, NW, SW, NW, NW, SE, W, W, NW, SE, NE],
          [SE, W, NE, NE, NE, NE, SE, NW, SE, W, NE, NW, W, W, SE],
          [W, E, NW, W, W, E, SE, E, E, W, E, SW, W, W, NW, W, E],
          [W, SW, E, E, SE, NE, NE, W, NW, W, NW, SE, NE, W, SE, NW, W, SE, SE, SE, NW, NE],
          [NE, E, SW, SE, E, NW, W, SW, NW, SW, SW, NW],
          [NE, NW, SW, W, SE, W, SW, NE, NE, NE, W, SE, NW, SE, NW, NE, SE, SE, NE, W],
          [E, NE, W, NW, E, W, NE, SW, SE, W, NW, SW, E, NW, E, SW, NE, NW, SE, NW, SW],
          [SW, E, NE, SW, NE, SW, NE, NE, E, NW, NE, W, E, NE, W, W, NE, SW, SW, NE, SE],
          [SW, W, E, SE, NE, SE, W, E, NW, NE, SW, NW, W, NE, SE, SW, W, NE],
          [E, NE, SE, NW, SW, W, SW, NE, NE, SW, SE, NW, NE, W, SW, SE, E, NW, SE, SE],
          [W, NW, NE, SE, NE, SE, NE, NW, W, NE, NW, SE, W, E, SE, W, SE, SE, SE, W],
          [NE, NE, W, SW, NW, E, W, SW, NE, NE, SE, NW, NE, SE, W, E, SW],
          [E, NE, SW, NW, SW, NW, SE, NE, NW, NW, NW, W, SE, E, SW, NE, E, W, SE, NE, SE],
          [NE, SW, NW, E, W, NW, NW, SE, E, NW, SE, E, SE, W, SE, NW, SW, E, E, W, E],
          [W, SE, W, E, E, E, NW, NE, SE, NW, W, W, SW, NE, W]
        ]
  describe "parseTilePaths" $ do
    it "parses the example" $ do
      input <- readFile "./test/Day24/example.txt"
      parseTilePaths input `shouldBe` Right exampleTilePaths
  let coordinates =
        [ Coordinates (-3, 1, 2),
          Coordinates (1, 2, -3),
          Coordinates (-3, 0, 3),
          Coordinates (2, 0, -2),
          Coordinates (1, 1, -2),
          Coordinates (-1, 1, 0),
          Coordinates (1, 2, -3),
          Coordinates (-2, 2, 0),
          Coordinates (0, 1, -1),
          Coordinates (-2, 1, 1),
          Coordinates (0, 2, -2),
          Coordinates (0, 2, -2),
          Coordinates (3, 0, -3),
          Coordinates (-1, 1, 0),
          Coordinates (0, -2, 2),
          Coordinates (0, 0, 0),
          Coordinates (1, 1, -2),
          Coordinates (2, 0, -2),
          Coordinates (2, -2, 0),
          Coordinates (-1, 2, -1)
        ]
  describe "asCoordinates" $ do
    it "reduces a materialized path into coordinates" $ do
      asCoordinates [NW, W, SW, E, E] `shouldBe` Coordinates (0, 0, 0)
    it "reduces an empty list to (0,0,0)" $ do
      asCoordinates [] `shouldBe` Coordinates (0, 0, 0)

    it "works on the example" $ do
      map asCoordinates exampleTilePaths `shouldBe` coordinates

  let exampleTileMap =
        Map.fromList
          [ (Coordinates (-3, 1, 2), Black),
            (Coordinates (-3, 0, 3), Black),
            (Coordinates (-1, 1, 0), White),
            (Coordinates (1, 2, -3), White),
            (Coordinates (-2, 2, 0), Black),
            (Coordinates (0, 1, -1), Black),
            (Coordinates (-2, 1, 1), Black),
            (Coordinates (0, 2, -2), White),
            (Coordinates (3, 0, -3), Black),
            (Coordinates (0, -2, 2), Black),
            (Coordinates (0, 0, 0), Black),
            (Coordinates (1, 1, -2), White),
            (Coordinates (2, 0, -2), White),
            (Coordinates (2, -2, 0), Black),
            (Coordinates (-1, 2, -1), Black)
          ]
  describe "asTileMap" $ do
    it "is the result of flipping tiles" $ do
      asTileMap coordinates `shouldBe` exampleTileMap
    it "has 10 Black tiles" $ do
      occurrences Black exampleTileMap `shouldBe` 10
  describe "livingArtDay" $ do
    -- TODO: build this with accumulator to make this test faster
    let cases =
          [ (0, 10),
            (1, 15),
            (2, 12),
            (3, 25),
            (4, 14),
            (5, 23),
            (6, 28),
            (7, 41),
            (8, 37),
            (9, 49),
            (10, 37),
            (20, 132),
            (30, 259),
            (40, 406),
            (50, 566),
            (60, 788),
            (70, 1106),
            (80, 1373),
            (90, 1844),
            (100, 2208)
          ] ::
            [(Int, Int)]
    let test (n, expected) = it ("Day " ++ show n ++ ": " ++ show expected) $ do
          (occurrences Black . livingArtDay n) exampleTileMap `shouldBe` expected

    for_ cases test
