module Day05.Solution (part1, part2, Seat (..), decode) where

import Control.Arrow (Arrow ((***)))
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . maximum . map (seatId . decode) . lines

part2 :: String -> String
part2 = show . missingSeatId . fromJust . find missingSeat . tripletWithNeighbors . sort . map (seatId . decode) . lines
  where
    tripletWithNeighbors ns = zip3 ns (drop 1 ns) (drop 2 ns)
    missingSeat (a, b, c) = not (succ a == b && succ b == c)
    missingSeatId (_, b, _) = succ b

data Seat = Seat {seatRow :: Int, seatCol :: Int, seatId :: Int} deriving (Show, Eq)

decode :: String -> Seat
decode = asSeat . (decode' 'B' *** decode' 'R') . splitAt 7
  where
    asSeat :: (Int, Int) -> Seat
    asSeat (row, col) = Seat {seatRow = row, seatCol = col, seatId = row * 8 + col}

decode' :: Char -> String -> Int
decode' char = go
  where
    go [] = 0
    go (x : xs)
      | x == char = 1 * (2 ^ length xs) + go xs
      | otherwise = go xs