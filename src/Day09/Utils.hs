module Day09.Utils where

import Advent.Utils (readInt)

parseNumbers :: String -> [Int]
parseNumbers = map readInt . lines

rollingChunks :: Int -> [a] -> [[a]]
rollingChunks size xs =
  [ take size . drop i $ xs
    | i <- [0 .. (length xs - size)]
  ]
