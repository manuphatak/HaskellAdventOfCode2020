module Day01.Solution (part1, part2, goalSeek) where

import Control.Monad ((<=<))
import Data.Foldable (Foldable (foldl'), find)
import Data.List (tails)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . fromJust . goalSeek 2 2020 . map readInt . lines

part2 :: String -> String
part2 = show . fromJust . goalSeek 3 2020 . map readInt . lines

readInt :: String -> Int
readInt n = read n :: Int

goalSeek :: Int -> Int -> [Int] -> Maybe Int
goalSeek n target = maybeProduct <=< find sumEqualGoals . combinations n
  where
    sumEqualGoals :: [Int] -> Bool
    sumEqualGoals ns = sum ns == target
    maybeProduct :: [Int] -> Maybe Int
    maybeProduct = Just . foldl' (*) 1

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys
    | y : xs' <- tails xs,
      ys <- combinations (n -1) xs'
  ]
