module Day01.Solution (part1, part2, goalSeek) where

import Advent.Utils (combinations, readInt)
import Data.List (find)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . fromJust . goalSeek 2 2020 . map readInt . lines

part2 :: String -> String
part2 = show . fromJust . goalSeek 3 2020 . map readInt . lines

goalSeek :: Int -> Int -> [Int] -> Maybe Int
goalSeek n target = fmap product . find ((target ==) . sum) . combinations n
