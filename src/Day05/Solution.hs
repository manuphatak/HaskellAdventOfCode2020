module Day05.Solution (part1, part2, Seat (..), decode) where

-- part1 :: String -> String

import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromJust)

part1 = show . maximum . map (seatId . decode) . lines

part2 = show . missingSeatId . fromJust . find missingSeat . tripletWithNeighbors . sort . map (seatId . decode) . lines
  where
    tripletWithNeighbors ns = zip3 ns (drop 1 ns) (drop 2 ns)
    missingSeat (a, b, c) = not (a + 1 == b && b + 1 == c)
    missingSeatId (a, b, _)
      | a + 1 == b = b + 1
      | otherwise = a + 1

data Seat = Seat {seatRow :: Int, seatCol :: Int, seatId :: Int} deriving (Show, Eq)

decode :: String -> Seat
decode = asSeat . decode' . splitAt 7
  where
    decode' :: (String, String) -> (Int, Int)
    decode' (row, col) = (decodeRow row, decodeCol col)
    asSeat :: (Int, Int) -> Seat
    asSeat (row, col) = Seat {seatRow = row, seatCol = col, seatId = row * 8 + col}
    decodeRow :: String -> Int
    decodeRow [] = 0
    decodeRow (x : xs)
      | x == 'B' = 1 * (2 ^ length xs) + decodeRow xs
      | otherwise = decodeRow xs

    decodeCol :: String -> Int
    decodeCol [] = 0
    decodeCol (x : xs)
      | x == 'R' = 1 * (2 ^ length xs) + decodeCol xs
      | otherwise = decodeCol xs
