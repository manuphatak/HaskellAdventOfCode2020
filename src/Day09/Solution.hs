module Day09.Solution (part1, part2, xmasCypher, encryptionWeakness, rollingChunks') where

import Advent.Utils (combinations)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Day09.Utils (parseNumbers, rollingChunks)

part1 :: String -> String
part1 = show . fromJust . xmasCypher 25

part2 :: String -> String
part2 = show . (encryptionWeakness <$> (fromJust . xmasCypher 25) <*> parseNumbers)

xmasCypher :: Int -> String -> Maybe Int
xmasCypher size = fmap snd . find (uncurry followsPreamble) . rollingChunks' size . parseNumbers
  where
    followsPreamble :: [Int] -> Int -> Bool
    followsPreamble preamble target = not $ any ((== target) . sum) (combinations 2 preamble)

rollingChunks' :: Int -> [a] -> [([a], a)]
rollingChunks' size = zip <$> rollingChunks size <*> drop size

encryptionWeakness :: Int -> [Int] -> Int
encryptionWeakness target = go 1
  where
    go :: Int -> [Int] -> Int
    go size ns
      | sum contiguousRange < target = go (succ size) ns
      | sum contiguousRange == target = minimum contiguousRange + maximum contiguousRange
      | sum contiguousRange > target = go 1 (tail ns)
      | otherwise = error "this should never happen"
      where
        contiguousRange = take size ns
