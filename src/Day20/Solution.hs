module Day20.Solution
  ( Grid,
    Image,
    Point,
    Tile (..),
    boundingBox,
    buildGrid,
    combineImages,
    cornerIds,
    countSeaMonsters,
    maxSeaMonsterCount,
    orientations,
    parseTiles,
    part1,
    part2,
    stitchedImage,
    stripEdges,
    tileFit,
  )
where

import Advent.Parser (intParser)
import Advent.Utils (fromRightOrShowError, occurrences)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import Data.Sequence (Seq (Empty, (:<|)), fromList, (|>))
import qualified Data.Set as Set
import Text.Parsec hiding (Empty)

part1 :: String -> String
part1 = show . product . cornerIds . buildGrid . fromRightOrShowError . parseTiles

part2 :: String -> String
part2 = show . waterRoughness . ((,) =<< maxSeaMonsterCount) . stitchedImage . buildGrid . fromRightOrShowError . parseTiles

type Image = [String]

data Tile = Tile {tId :: Int, tContent :: Image} deriving (Show, Eq)

parseTiles :: String -> Either ParseError (Seq Tile)
parseTiles = parse (tilesParser <* eof) ""
  where
    tilesParser :: Parsec String () (Seq Tile)
    tilesParser = fromList <$> tileParser `sepEndBy1` newline

    tileParser :: Parsec String () Tile
    tileParser = Tile <$> (string "Tile " *> intParser <* char ':' <* newline) <*> imageParser

    imageParser :: Parsec String () Image
    imageParser = many1 (oneOf ['.', '#']) `sepEndBy1` newline

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
buildGrid Empty = Map.empty
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
    (minX, maxX, minY, maxY) = bounds grid

bounds :: Grid -> (Int, Int, Int, Int)
bounds grid =
  ( gridKeys & map fst & minimum,
    gridKeys & map fst & maximum,
    gridKeys & map snd & minimum,
    gridKeys & map snd & maximum
  )
  where
    gridKeys :: [Point]
    gridKeys = Map.keys grid

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
    [ point & add (0, -1) & flip Map.lookup grid <&> tDown & isMatchOn tUp tile,
      point & add (0, 1) & flip Map.lookup grid <&> tUp & isMatchOn tDown tile,
      point & add (1, 0) & flip Map.lookup grid <&> tLeft & isMatchOn tRight tile,
      point & add (-1, 0) & flip Map.lookup grid <&> tRight & isMatchOn tLeft tile
    ]
  where
    isMatchOn :: Eq a => (t -> a) -> t -> Maybe a -> Bool
    isMatchOn _ _ Nothing = True
    isMatchOn fn t (Just s) = fn t == s

orientations :: Image -> [Image]
orientations xs = (take 4 . iterate (transpose . reverse) $ xs) ++ (take 4 . iterate (transpose . reverse) $ mirror)
  where
    mirror = map reverse xs

stitchedImage :: Grid -> Image
stitchedImage grid = combineImages . map (\y -> map (\x -> grid Map.! (x, y) & tContent & stripEdges) [minX .. maxX]) $ [minY .. maxY]
  where
    (minX, maxX, minY, maxY) = bounds grid

stripEdges :: Image -> Image
stripEdges = map (init . tail) . init . tail

combineImages :: Semigroup c => [[[c]]] -> [c]
combineImages = concatMap (foldr1 (zipWith (<>)))

maxSeaMonsterCount :: Image -> Int
maxSeaMonsterCount = maximum . map countSeaMonsters . orientations

waterRoughness :: (Int, Image) -> Int
waterRoughness (c, img) = img & map (occurrences '#') & sum & subtract (seaMonsterCharCount * c)

seaMonsterCharCount :: Int
seaMonsterCharCount = seaMonsterMatcher & Map.size

seaMonster :: Image
seaMonster =
  [ "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ]

imageDimensions :: Image -> (Int, Int)
imageDimensions img = ((length . head) img, length img)

type ImageMap = Map.Map Point Char

toImageMap :: Image -> ImageMap
toImageMap =
  Map.fromList
    . concatMap
      ( \(y, rows) ->
          rows
            & zip [0 ..]
            & map
              ( \(x, cell) ->
                  ((x, y), cell)
              )
      )
    . zip [0 ..]

seaMonsterMatcher :: ImageMap
seaMonsterMatcher = seaMonster & toImageMap & Map.filter (/= ' ')

countSeaMonsters :: Image -> Int
countSeaMonsters img = filter isSeaMonster startPoints & length
  where
    startPoints :: [Point]
    startPoints = [(x, y) | x <- [0 .. (imgW - monsterW)], y <- [0 .. (imgH - monsterH)]]

    isSeaMonster :: Point -> Bool
    isSeaMonster point = Map.mapWithKey (isMatch point) seaMonsterMatcher & and

    isMatch :: Point -> Point -> Char -> Bool
    isMatch l r m = Map.lookup (l `add` r) imageMap == Just m

    imageMap :: ImageMap
    imageMap = toImageMap img

    (imgW, imgH) = imageDimensions img
    (monsterW, monsterH) = imageDimensions seaMonster
