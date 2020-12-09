module Day09.Solution (part1, part2, xmasCypher, encryptionWeakness, parseNumbers, rollingChunks) where

import Advent.Utils (combinations, readInt)
import Data.Foldable (find)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . fromJust . xmasCypher 25

part2 :: String -> String
part2 = show . (encryptionWeakness <$> (fromJust . xmasCypher 25) <*> parseNumbers)

xmasCypher :: Int -> String -> Maybe Int
xmasCypher size = fmap snd . find (uncurry followsPreamble) . rollingChunks size . parseNumbers
  where
    followsPreamble :: [Int] -> Int -> Bool
    followsPreamble preamble target = not $ any ((== target) . sum) (combinations 2 preamble)

parseNumbers :: String -> [Int]
parseNumbers = map readInt . lines

rollingChunks :: Int -> [a] -> [([a], a)]
rollingChunks size xs =
  [ (take size chunkStart, chunkStart !! max 0 size)
    | i <- [0 .. (length xs - (size + 1))],
      let chunkStart = drop i xs
  ]

encryptionWeakness :: Int -> [Int] -> Int
encryptionWeakness target = go 1
  where
    go :: Int -> [Int] -> Int
    go _ [] = error "this should never happen"
    go size ns
      | sum contiguousRange < target = go (succ size) ns
      | sum contiguousRange == target = minimum contiguousRange + maximum contiguousRange
      | sum contiguousRange > target = go 1 (tail ns)
      where
        contiguousRange = take size ns
