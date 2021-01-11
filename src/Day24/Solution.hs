module Day24.Solution where

import Advent.Utils
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences Black . asTileMap . map asCoordinates . fromRightOrShowError . parseTilePaths

part2 :: String -> String
part2 = head . lines

data Neighbor = E | SE | SW | W | NW | NE deriving (Show, Eq, Read)

type TilePath = [Neighbor]

newtype Coordinates = Coordinates {getCoordinates :: (Int, Int, Int)} deriving (Show, Eq, Ord)

instance Semigroup Coordinates where
  (Coordinates (a, b, c)) <> (Coordinates (x, y, z)) = Coordinates (a + x, b + y, c + z)

instance Monoid Coordinates where
  mempty = Coordinates (0, 0, 0)

data TileState = White | Black deriving (Show, Eq)

parseTilePaths :: String -> Either ParseError [TilePath]
parseTilePaths = parse (tilePathParser `sepEndBy1` newline) ""
  where
    tilePathParser :: Parsec String () TilePath
    tilePathParser = many1 neighborParser

    neighborParser :: Parsec String () Neighbor
    neighborParser =
      read . map toUpper
        <$> choice
          [ try $ string "e",
            try $ string "se",
            try $ string "sw",
            try $ string "w",
            try $ string "nw",
            try $ string "ne"
          ]

asCoordinates :: TilePath -> Coordinates
asCoordinates = mconcat . map go
  where
    go :: Neighbor -> Coordinates
    go E = Coordinates (1, -1, 0)
    go SE = Coordinates (0, -1, 1)
    go SW = Coordinates (-1, 0, 1)
    go W = Coordinates (-1, 1, 0)
    go NW = Coordinates (0, 1, -1)
    go NE = Coordinates (1, 0, -1)

asTileMap :: [Coordinates] -> Map Coordinates TileState
asTileMap = foldr go Map.empty
  where
    go :: Coordinates -> Map Coordinates TileState -> Map Coordinates TileState
    go c m = Map.insertWith flipTile c Black m

    flipTile :: TileState -> TileState -> TileState
    flipTile _ White = Black
    flipTile _ Black = White
