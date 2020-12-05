module Day05.Solution (part1, part2, Seat (..), decode) where

-- part1 :: String -> String
part1 = show . maximum . map (seatId . decode) . lines

part2 :: String -> String
part2 = id

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

-- >>> input <- readFile "./test/Day05/input.txt"
-- >>> part1 input
-- 998
