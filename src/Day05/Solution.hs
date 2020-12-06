module Day05.Solution (part1, part2, Seat (..), decode) where

import Control.Arrow (Arrow ((***)))
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . maximum . map (seatId . decode) . lines

part2 :: String -> String
part2 = show . missingSeatId . fromJust . find isMissingSeat . tripletWithNeighbors . sort . map (seatId . decode) . lines
  where
    tripletWithNeighbors :: [Int] -> [(Int, Int, Int)]
    tripletWithNeighbors ns = zip3 ns (drop 1 ns) (drop 2 ns)
    isMissingSeat :: (Int, Int, Int) -> Bool
    isMissingSeat (a, b, c) = not (succ a == b && succ b == c)
    missingSeatId :: (Int, Int, Int) -> Int
    missingSeatId (_, b, _) = succ b

data Seat = Seat {seatRow :: Int, seatCol :: Int, seatId :: Int} deriving (Show, Eq)

decode :: String -> Seat
decode = asSeat . (asInt 'B' *** asInt 'R') . splitAt 7
  where
    asSeat :: (Int, Int) -> Seat
    asSeat (row, col) = Seat {seatRow = row, seatCol = col, seatId = row * 8 + col}

-- asInt 'B' "BFFFBBF"
--   == Binary 1000110
--   == (1 x 2^6) + (0 x 2^5) + (0 x 2^4) + (0 x 2^3) + (1 x 2^2) + (1 x 2^1) + (0 x 2^0)
--   == Int 70
asInt :: Char -> String -> Int
asInt char = go
  where
    go [] = 0
    go (x : xs)
      | x == char = (2 ^ length xs) + go xs
      | otherwise = go xs
