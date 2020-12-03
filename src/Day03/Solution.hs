module Day03.Solution (part1, part2, followSlope) where

part1 :: String -> String
part1 = show . followSlope 3 1

part2 :: String -> String
part2 = show . candidateSlopes

candidateSlopes :: String -> Int
candidateSlopes input = product [followSlope 1 1 input, followSlope 3 1 input, followSlope 5 1 input, followSlope 7 1 input, followSlope 1 2 input]

data Cell = T | E deriving (Eq, Show)

type Row = [Cell]

type Grid = [Row]

data Coordinates = Coordinates Int Int deriving (Show)

type TreeCount = Int

data Projection = Projection TreeCount Coordinates Grid

instance Show Projection where
  show (Projection treeCount coordinates _) = "(Projection " ++ show treeCount ++ " (" ++ show coordinates ++ ") Grid)"

followSlope :: Int -> Int -> String -> Int
followSlope slopeX slopY = projectionTreeCount . walk slopeX slopY . asProjection . map (map readCell) . lines

projectionTreeCount :: Projection -> TreeCount
projectionTreeCount (Projection treeCount _ _) = treeCount

walk :: Int -> Int -> Projection -> Projection
walk slopeX slopeY projection@(Projection _ (Coordinates _ y) grid)
  | y == (length grid - 1) = projection
  | otherwise = walk slopeX slopeY (step slopeX slopeY projection)

step :: Int -> Int -> Projection -> Projection
step slopeX slopeY (Projection treeCount (Coordinates x y) grid) =
  let x' = x + slopeX
      y' = y + slopeY
      cell = (grid !! y') !! x'
      nextTreeCount = if cell == T then succ treeCount else treeCount
   in Projection nextTreeCount (Coordinates x' y') grid

asProjection :: Grid -> Projection
asProjection grid = Projection 0 (Coordinates 0 0) (map cycle grid)

readCell :: Char -> Cell
readCell '.' = E
readCell '#' = T
readCell _ = error "unknown Cell"
