module Day24.Solution where

import Advent.Utils
import Data.Char
import Data.Function
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Prelude hiding (lookup)

part1 :: String -> String
part1 = show . occurrences Black . asTileMap . map asCoordinates . fromRightOrShowError . parseTilePaths

part2 :: String -> String
part2 = show . occurrences Black . (IntMap.! 100) . livingArtDay 100 . asTileMap . map asCoordinates . fromRightOrShowError . parseTilePaths

data Neighbor = E | SE | SW | W | NW | NE deriving (Show, Eq, Read)

type TilePath = [Neighbor]

newtype Coordinates = Coordinates {getCoordinates :: (Int, Int, Int)} deriving (Show, Eq, Ord)

instance Semigroup Coordinates where
  (Coordinates (a, b, c)) <> (Coordinates (x, y, z)) = Coordinates (a + x, b + y, c + z)

instance Monoid Coordinates where
  mempty = Coordinates (0, 0, 0)

data TileState = White | Black deriving (Show, Eq)

type TileMap = Map Coordinates TileState

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

asTileMap :: [Coordinates] -> TileMap
asTileMap = foldr go Map.empty
  where
    go :: Coordinates -> TileMap -> TileMap
    go c m = Map.insertWith flipTile c Black m

    flipTile :: TileState -> TileState -> TileState
    flipTile _ White = Black
    flipTile _ Black = White

livingArtDay :: Int -> TileMap -> IntMap TileMap
livingArtDay d = go IntMap.empty 0
  where
    go :: IntMap TileMap -> Int -> TileMap -> IntMap TileMap
    go history n tileMap
      | n < 0 = undefined
      | n > d = history
      | n == 0 = go (IntMap.insert n tileMap history) (n + 1) tileMap
      | otherwise = go (IntMap.insert n nextTileMap history) (n + 1) nextTileMap
      where
        nextTileMap :: TileMap
        nextTileMap = foldr foldNext tileMap candidateTiles

        foldNext :: Coordinates -> TileMap -> TileMap
        foldNext coordinates = insert coordinates (nextTile currentTile neighborTiles)
          where
            currentTile :: TileState
            currentTile = lookup coordinates tileMap
            neighborTiles :: [TileState]
            neighborTiles = map (`lookup` tileMap) (neighbors' coordinates)

        nextTile :: TileState -> [TileState] -> TileState
        nextTile Black neighborTiles
          | neighborTiles & occurrences Black & ((||) <$> (0 ==) <*> (> 2)) = White
          | otherwise = Black
        nextTile White neighborTiles
          | neighborTiles & occurrences Black & (== 2) = Black
          | otherwise = White

        candidateTiles :: Set Coordinates
        candidateTiles = Set.union (Map.keysSet tileMap) . Set.unions . Set.map (Set.fromList . neighbors') . Map.keysSet $ tileMap

insert :: Ord k => k -> TileState -> Map k TileState -> Map k TileState
insert k White = Map.delete k
insert k Black = Map.insert k Black

lookup :: Coordinates -> TileMap -> TileState
lookup = Map.findWithDefault White

neighbors :: [Coordinates]
neighbors =
  [ Coordinates (x, y, z)
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      z <- [-1 .. 1],
      x + y + z == 0,
      (x, y, z) /= (0, 0, 0)
  ]

neighbors' :: Coordinates -> [Coordinates]
neighbors' coordinates = map (<> coordinates) neighbors
