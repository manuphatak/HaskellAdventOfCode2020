module Day24.Solution where

import Advent.Utils
import Data.Char
import Data.Function
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Prelude hiding (lookup)

part1 :: String -> String
part1 = show . length . asFlippedTileSet . map asCoordinates . fromRightOrShowError . parseTilePaths

part2 :: String -> String
part2 = show . length . (IntMap.! 100) . livingArtDay 100 . asFlippedTileSet . map asCoordinates . fromRightOrShowError . parseTilePaths

data Neighbor = E | SE | SW | W | NW | NE deriving (Show, Eq, Read)

type TilePath = [Neighbor]

newtype Coordinates = Coordinates {getCoordinates :: (Int, Int, Int)} deriving (Show, Eq, Ord)

instance Semigroup Coordinates where
  (Coordinates (a, b, c)) <> (Coordinates (x, y, z)) = Coordinates (a + x, b + y, c + z)

instance Monoid Coordinates where
  mempty = Coordinates (0, 0, 0)

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

asFlippedTileSet :: [Coordinates] -> Set Coordinates
asFlippedTileSet = foldr toggle Set.empty

livingArtDay :: Int -> Set Coordinates -> IntMap (Set Coordinates)
livingArtDay d = go IntMap.empty 0
  where
    go :: IntMap (Set Coordinates) -> Int -> Set Coordinates -> IntMap (Set Coordinates)
    go history n flippedTileSet
      | n < 0 = undefined
      | n > d = history
      | n == 0 = go (IntMap.insert n flippedTileSet history) (n + 1) flippedTileSet
      | otherwise = go (IntMap.insert n nextFlippedTileSet history) (n + 1) nextFlippedTileSet
      where
        nextFlippedTileSet :: Set Coordinates
        nextFlippedTileSet = foldr nextTileState flippedTileSet candidateTiles

        nextTileState :: Coordinates -> Set Coordinates -> Set Coordinates
        nextTileState point
          | isMember && (neighborTiles & length & ((||) <$> (0 ==) <*> (> 2))) = Set.delete point
          | isMember = Set.insert point
          | not isMember && (neighborTiles & length & (== 2)) = Set.insert point
          | not isMember = Set.delete point
          | otherwise = undefined
          where
            isMember :: Bool
            isMember = Set.member point flippedTileSet
            neighborTiles :: Set Coordinates
            neighborTiles = Set.filter (`Set.member` flippedTileSet) $ candidates point

        -- nextTileState :: Bool -> Set Coordinates -> Bool
        -- nextTileState = undefined
        -- nextTileState Black neighborTiles
        --   | neighborTiles & occurrences Black & ((||) <$> (0 ==) <*> (> 2)) = White
        --   | otherwise = Black
        -- nextTileState White neighborTiles
        --   | neighborTiles & occurrences Black & (== 2) = Black
        --   | otherwise = White

        candidateTiles :: Set Coordinates
        candidateTiles = Set.union flippedTileSet . Set.unions . Set.map candidates $ flippedTileSet

shouldToggle :: Bool -> Coordinates -> Set Coordinates -> Set Coordinates
shouldToggle = undefined

toggle :: Coordinates -> Set Coordinates -> Set Coordinates
toggle point set
  | Set.member point set = Set.delete point set
  | otherwise = Set.insert point set

candidateOffsets :: Set Coordinates
candidateOffsets =
  Set.fromList
    [ Coordinates (x, y, z)
      | x <- [-1 .. 1],
        y <- [-1 .. 1],
        z <- [-1 .. 1],
        x + y + z == 0,
        (x, y, z) /= (0, 0, 0)
    ]

candidates :: Coordinates -> Set Coordinates
candidates coordinates = Set.map (<> coordinates) candidateOffsets
