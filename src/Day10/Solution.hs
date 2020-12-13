module Day10.Solution (joltageJumps, part1, part2, possibilitiesTree) where

import Advent.Utils (parseInts)
import Data.List (sort)
import Data.Tree (Tree, unfoldTree)

part1 :: String -> String
part1 = show . (\(a, _, c) -> a * c) . joltageJumps

part2 :: String -> String
part2 = show . succ . sum . possibilitiesTree

joltageJumps :: String -> (Int, Int, Int)
joltageJumps = go (0, 0, 0) . sort . withMinMax . parseInts
  where
    go :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
    go jumps [_] = jumps
    go (a, b, c) (x : x' : xs) = go nextJumps (x' : xs)
      where
        nextJumps = case x' - x of
          1 -> (succ a, b, c)
          2 -> (a, succ b, c)
          3 -> (a, b, succ c)

possibilitiesTree :: String -> Tree Int
possibilitiesTree = unfoldTree go . sort . withMinMax . parseInts
  where
    go :: [Int] -> (Int, [[Int]])
    go [] = (0, [])
    go [_] = (0, [])
    go (x : xs) =
      (pred $ length possibilities, possibilities)
      where
        possibilities :: [[Int]]
        possibilities =
          [ xs'
            | i <- [0 .. 2],
              let xs' = next x (drop i xs),
              not (null xs')
          ]

    next :: Int -> [Int] -> [Int]
    next _ [] = []
    next y ys
      | head ys - y <= 3 = ys
      | otherwise = []

withMinMax :: [Int] -> [Int]
withMinMax = (0 :) . ((:) =<< (3 +) . maximum)
