module Day10.Solution where

import Advent.Utils (parseInts)
import Data.List (sort)

part1 :: String -> String
part1 = show . (\(a, _, c) -> a * c) . joltageJumps

part2 :: String -> String
part2 = head . lines

joltageJumps :: String -> (Int, Int, Int)
joltageJumps = go (0, 0, 1) . (0 :) . sort . parseInts
  where
    go :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
    go jumps [_] = jumps
    go (a, b, c) (x : x' : xs) = go nextJumps (x' : xs)
      where
        nextJumps = case x' - x of
          1 -> (succ a, b, c)
          2 -> (a, succ b, c)
          3 -> (a, b, succ c)
