module Day20.Solution where

import Advent.Parser
import Advent.Utils
import Control.Monad
import Data.Foldable (find)
import Data.Function
import Data.Functor
import Data.List (minimumBy, transpose)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence hiding (reverse, take)
import qualified Data.Set as Set
import Text.Parsec hiding (Empty)

part1 :: String -> String
part1 = show . product . cornerIds . buildGrid . fromRightOrShowError . parseTiles

part2 :: String -> String
part2 = head . lines

data Tile = Tile {tId :: Int, tContent :: [[Char]]} deriving (Show, Eq)

parseTiles :: String -> Either ParseError (Seq Tile)
parseTiles = parse (tilesParser <* eof) ""
  where
    tilesParser :: Parsec String () (Seq Tile)
    tilesParser = fromList <$> tileParser `sepEndBy1` newline

    tileParser :: Parsec String () Tile
    tileParser = Tile <$> (string "Tile " *> intParser <* char ':' <* newline) <*> tileContentParser

    tileContentParser :: Parsec String () [[Char]]
    tileContentParser = many1 (oneOf ['.', '#']) `sepEndBy1` newline

tUp :: Tile -> String
tUp = head . tContent

tRight :: Tile -> String
tRight = map last . tContent

tDown :: Tile -> String
tDown = last . tContent

tLeft :: Tile -> String
tLeft = map head . tContent

type Point = (Int, Int)

type Grid = Map.Map Point Tile

buildGrid :: Seq Tile -> Grid
buildGrid (x :<| xs) = go (xs, Map.fromList [((0, 0), x)])
  where
    go :: (Seq Tile, Grid) -> Grid
    go (Empty, grid) = grid
    go (tile :<| tiles, grid)
      | tileOrientation & isJust = go (tiles, grid `withTile` tileOrientation)
      | otherwise = go (tiles |> tile, grid)
      where
        tileOrientation :: Maybe TileOrientation
        tileOrientation = candidatePoints & Set.toList & findTileOrientation

        findTileOrientation :: [Point] -> Maybe TileOrientation
        findTileOrientation [] = Nothing
        findTileOrientation (p : ps) = case tileFit grid p tile of
          Just tile' -> Just (p, tile')
          _ -> findTileOrientation ps

        candidatePoints :: Set.Set Point
        candidatePoints = gridKeys & Set.map neighbors & Set.unions & flip Set.difference gridKeys

        gridKeys :: Set.Set Point
        gridKeys = grid & Map.keys & Set.fromList

boundingBox :: Grid -> [Point]
boundingBox grid = [(minX, minY), (minX, maxY), (maxX, minY), (maxX, maxY)]
  where
    gridKeys = Map.keys grid
    minX :: Int
    minX = gridKeys & map fst & minimum
    maxX :: Int
    maxX = gridKeys & map fst & maximum
    minY :: Int
    minY = gridKeys & map snd & minimum
    maxY :: Int
    maxY = gridKeys & map snd & maximum

cornerIds :: Grid -> [Int]
cornerIds grid = grid & boundingBox & map (`Map.lookup` grid) & catMaybes & map tId

type TileOrientation = (Point, Tile)

withTile :: Grid -> Maybe TileOrientation -> Grid
grid `withTile` Just (point, tile) = Map.insert point tile grid
grid `withTile` Nothing = grid

neighbors :: Point -> Set.Set Point
neighbors point = neighborOffsets & map (add point) & Set.fromList
  where
    neighborOffsets :: [Point]
    neighborOffsets = [(1, 0), (-1, 0), (0, 1), (0, -1)]

add :: Point -> Point -> Point
add (a, b) (x, y) = (a + x, b + y)

tileFit :: Grid -> Point -> Tile -> Maybe Tile
tileFit grid point tile = find (tileFitsPoint grid point) tileOrientations
  where
    tileOrientations :: [Tile]
    tileOrientations = map (Tile (tId tile)) . orientations . tContent $ tile

tileFitsPoint :: Grid -> Point -> Tile -> Bool
tileFitsPoint grid point tile =
  and
    [ point & add (1, 0) & flip Map.lookup grid <&> tDown & isMatchOn tUp tile,
      point & add (-1, 0) & flip Map.lookup grid <&> tUp & isMatchOn tDown tile,
      point & add (0, 1) & flip Map.lookup grid <&> tLeft & isMatchOn tRight tile,
      point & add (0, -1) & flip Map.lookup grid <&> tRight & isMatchOn tLeft tile
    ]
  where
    isMatchOn :: Eq a => (t -> a) -> t -> Maybe a -> Bool
    isMatchOn _ t Nothing = True
    isMatchOn fn t (Just s) = fn t == s

orientations :: [[a]] -> [[[a]]]
orientations xs = (take 4 . iterate (transpose . reverse) $ xs) ++ (take 4 . iterate (transpose . reverse) $ mirror)
  where
    mirror = map reverse xs
