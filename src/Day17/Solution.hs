{-# LANGUAGE ScopedTypeVariables #-}

module Day17.Solution
  ( CubeState (..),
    Point3D (..),
    Point4D (..),
    executeCycles,
    parsePocketDimension3D,
    parsePocketDimension4D,
    part1,
    part2,
  )
where

import Advent.Utils
  ( fromRightOrShowError,
    isBetween,
    occurrences,
  )
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences Active . executeCycles 6 . fromRightOrShowError . parsePocketDimension3D

part2 :: String -> String
part2 = show . occurrences Active . executeCycles 6 . fromRightOrShowError . parsePocketDimension4D

class Pocket a where
  from2D :: (Int, Int) -> a
  offset :: a -> a -> a
  neighbors :: a -> Set.Set a

newtype Point3D = Point3D (Int, Int, Int) deriving (Eq, Ord, Show)

instance Pocket Point3D where
  from2D (x, y) = Point3D (x, y, 0)
  Point3D (a, b, c) `offset` Point3D (x, y, z) = Point3D (a + x, b + y, c + z)
  neighbors point =
    Set.fromList
      [ point `offset` Point3D (x, y, z)
        | (x, y, z) <- offsets3D,
          (x, y, z) /= (0, 0, 0)
      ]

newtype Point4D = Point4D (Int, Int, Int, Int) deriving (Eq, Ord, Show)

instance Pocket Point4D where
  from2D (x, y) = Point4D (x, y, 0, 0)
  Point4D (a, b, c, d) `offset` Point4D (x, y, z, w) = Point4D (a + x, b + y, c + z, d + w)
  neighbors point =
    Set.fromList
      [ point `offset` Point4D (x, y, z, w)
        | (x, y, z, w) <- offsets4D,
          (x, y, z, w) /= (0, 0, 0, 0)
      ]

offsets3D :: [(Int, Int, Int)]
offsets3D =
  [ (x, y, z)
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      z <- [-1 .. 1]
  ]

offsets4D :: [(Int, Int, Int, Int)]
offsets4D =
  [ (x, y, z, w)
    | (x, y, z) <- offsets3D,
      w <- [-1 .. 1]
  ]

data CubeState = Inactive | Active deriving (Show, Eq)

type PocketDimension a = Map.Map a CubeState

parsePocketDimension3D :: String -> Either ParseError (PocketDimension Point3D)
parsePocketDimension3D = parsePocketDimension

parsePocketDimension4D :: String -> Either ParseError (PocketDimension Point4D)
parsePocketDimension4D = parsePocketDimension

parsePocketDimension :: (Ord a, Pocket a) => String -> Either ParseError (PocketDimension a)
parsePocketDimension = parse pocketDimensionParser ""

pocketDimensionParser :: forall a. (Ord a, Pocket a) => Parsec String () (PocketDimension a)
pocketDimensionParser = pocketDimensionBuilder <$> rowParser `sepEndBy1` endOfLine
  where
    pocketDimensionBuilder :: [[CubeState]] -> PocketDimension a
    pocketDimensionBuilder rows =
      Map.fromList
        [ (from2D (x, y), cell)
          | (y, row) <- zip [0 ..] rows,
            (x, cell) <- zip [0 ..] row
        ]

    rowParser :: Parsec String () [CubeState]
    rowParser = many1 cubeStateParser

    cubeStateParser :: Parsec String () CubeState
    cubeStateParser = asCubeState <$> oneOf ['#', '.']

    asCubeState :: Char -> CubeState
    asCubeState '#' = Active
    asCubeState _ = Inactive

executeCycles :: forall a. (Ord a, Pocket a) => Int -> PocketDimension a -> PocketDimension a
executeCycles 0 pocketDimension = pocketDimension
executeCycles i pocketDimension = executeCycles (pred i) nextPocketDimension
  where
    nextPocketDimension :: PocketDimension a
    nextPocketDimension =
      Map.fromList
        [ (point, Active)
          | point <- allPossiblePoints,
            Active == cycleCubeState point (findCube point)
        ]

    allPossiblePoints :: [a]
    allPossiblePoints = Set.toList . Set.unions . map neighbors . Map.keys . Map.filter (== Active) $ pocketDimension

    cycleCubeState :: a -> CubeState -> CubeState
    cycleCubeState point Active
      | neighbors point & Set.toList & map findCube & occurrences Active & isBetween 2 3 = Active
      | otherwise = Inactive
    cycleCubeState point Inactive
      | neighbors point & Set.toList & map findCube & occurrences Active & (== 3) = Active
      | otherwise = Inactive

    findCube :: a -> CubeState
    findCube point = Map.findWithDefault Inactive point pocketDimension
