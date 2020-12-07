module Day03.Solution (part1, part2, slopePath) where

import Advent.Utils (occurrences)

part1 :: String -> String
part1 = show . occurrences '#' . slopePath 3 1 . lines

part2 :: String -> String
part2 input = show . product $ map treesInSlope slopes
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    treesInSlope :: (Int, Int) -> Int
    treesInSlope (x, y) = occurrences '#' . slopePath x y . lines $ input

slopePath :: Int -> Int -> [String] -> String
slopePath x y = go . map cycle
  where
    go grid
      | y < length grid = ((:) <$> head . head <*> go) . map (drop x) . drop y $ grid
      | otherwise = []
