module Day17.Solution where

import Advent.Utils
import Data.Function
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences Active . executeCycles 6 . fromRightOrShowError . parsePocketDimension

part2 :: String -> String
part2 = head . lines

type Point = (Int, Int, Int)

data CubeState = Inactive | Active deriving (Show, Eq)

type PocketDimension = Map.Map Point CubeState

parsePocketDimension :: String -> Either ParseError PocketDimension
parsePocketDimension = parse pocketDimensionParser ""

pocketDimensionParser :: Parsec String () PocketDimension
pocketDimensionParser = pocketDimensionBuilder <$> rowParser `sepEndBy1` endOfLine
  where
    pocketDimensionBuilder :: [[CubeState]] -> PocketDimension
    pocketDimensionBuilder rows =
      Map.fromList
        [ ((x, y, 0), cell)
          | (y, row) <- zip [0 ..] rows,
            (x, cell) <- zip [0 ..] row
        ]

    rowParser :: Parsec String () [CubeState]
    rowParser = many1 cubeStateParser

    cubeStateParser :: Parsec String () CubeState
    cubeStateParser = asCubeState <$> oneOf ['#', '.']

    asCubeState :: Char -> CubeState
    asCubeState '#' = Active
    asCubeState '.' = Inactive

executeCycles :: Int -> PocketDimension -> PocketDimension
executeCycles 0 pocketDimension = pocketDimension
executeCycles i pocketDimension = executeCycles (pred i) nextPocketDimension
  where
    nextPocketDimension :: PocketDimension
    nextPocketDimension =
      Map.fromList
        [ (point, Active)
          | point <- allPossiblePoints,
            Active == cycleCubeState point (findCube point)
        ]

    allPossiblePoints :: [Point]
    allPossiblePoints = Set.toList . Set.unions . map neighbors . Map.keys . Map.filter (== Active) $ pocketDimension

    cycleCubeState :: Point -> CubeState -> CubeState
    cycleCubeState point Active
      | neighbors point & Set.toList & map findCube & occurrences Active & isBetween 2 3 = Active
      | otherwise = Inactive
    cycleCubeState point Inactive
      | neighbors point & Set.toList & map findCube & occurrences Active & (== 3) = Active
      | otherwise = Inactive

    neighbors :: Point -> Set.Set Point
    neighbors point =
      Set.fromList
        [ key
          | x <- [-1 .. 1],
            y <- [-1 .. 1],
            z <- [-1 .. 1],
            (x, y, z) /= (0, 0, 0),
            let key = point `offset` ((x, y, z) :: Point)
        ]

    findCube :: Point -> CubeState
    findCube point = Map.findWithDefault Inactive point pocketDimension

offset :: Point -> Point -> Point
(a, b, c) `offset` (x, y, z) = (a + x, b + y, c + z)
