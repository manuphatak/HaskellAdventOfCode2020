module Day17.Solution where

import Advent.Utils
import Data.Function
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences Active . executeCycles 6 . fromRightOrShowError . parsePocketDimension

part2 :: String -> String
part2 = show . occurrences Active . executeCyclesV2 6 . fromRightOrShowError . parsePocketDimensionV2

data CubeState = Inactive | Active deriving (Show, Eq)

type Point = (Int, Int, Int)

type PointV2 = (Int, Int, Int, Int)

type PocketDimension = Map.Map Point CubeState

type PocketDimensionV2 = Map.Map PointV2 CubeState

parsePocketDimension :: String -> Either ParseError PocketDimension
parsePocketDimension = parse pocketDimensionParser ""

parsePocketDimensionV2 :: String -> Either ParseError PocketDimensionV2
parsePocketDimensionV2 = parse pocketDimensionParserV2 ""

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

pocketDimensionParserV2 :: Parsec String () PocketDimensionV2
pocketDimensionParserV2 = pocketDimensionBuilder <$> rowParser `sepEndBy1` endOfLine
  where
    pocketDimensionBuilder :: [[CubeState]] -> PocketDimensionV2
    pocketDimensionBuilder rows =
      Map.fromList
        [ ((x, y, 0, 0), cell)
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
        [ point `offset` ((x, y, z) :: Point)
          | x <- [-1 .. 1],
            y <- [-1 .. 1],
            z <- [-1 .. 1],
            (x, y, z) /= (0, 0, 0)
        ]

    findCube :: Point -> CubeState
    findCube point = Map.findWithDefault Inactive point pocketDimension

executeCyclesV2 :: Int -> PocketDimensionV2 -> PocketDimensionV2
executeCyclesV2 0 pocketDimension = pocketDimension
executeCyclesV2 i pocketDimension = executeCyclesV2 (pred i) nextPocketDimension
  where
    nextPocketDimension :: PocketDimensionV2
    nextPocketDimension =
      Map.fromList
        [ (point, Active)
          | point <- allPossiblePoints,
            Active == cycleCubeState point (findCube point)
        ]

    allPossiblePoints :: [PointV2]
    allPossiblePoints = Set.toList . Set.unions . map neighbors . Map.keys . Map.filter (== Active) $ pocketDimension

    cycleCubeState :: PointV2 -> CubeState -> CubeState
    cycleCubeState point Active
      | neighbors point & Set.toList & map findCube & occurrences Active & isBetween 2 3 = Active
      | otherwise = Inactive
    cycleCubeState point Inactive
      | neighbors point & Set.toList & map findCube & occurrences Active & (== 3) = Active
      | otherwise = Inactive

    neighbors :: PointV2 -> Set.Set PointV2
    neighbors point =
      Set.fromList
        [ point `offsetV2` ((x, y, z, w) :: PointV2)
          | x <- [-1 .. 1],
            y <- [-1 .. 1],
            z <- [-1 .. 1],
            w <- [-1 .. 1],
            (x, y, z, w) /= (0, 0, 0, 0)
        ]

    findCube :: PointV2 -> CubeState
    findCube point = Map.findWithDefault Inactive point pocketDimension

offset :: Point -> Point -> Point
(a, b, c) `offset` (x, y, z) = (a + x, b + y, c + z)

offsetV2 :: PointV2 -> PointV2 -> PointV2
(a, b, c, d) `offsetV2` (x, y, z, w) = (a + x, b + y, c + z, d + w)
