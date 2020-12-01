module Day01.Solution (part1, part2, goalSeek) where

import Control.Monad ((<=<))
import Data.Foldable (find)
import Data.List (tails)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . fromJust . goalSeek 2020 . map readInt . lines

part2 :: String -> String
part2 = id

readInt :: String -> Int
readInt n = read n :: Int

goalSeek :: Int -> [Int] -> Maybe Int
goalSeek target = uncurry maybeProduct <=< find (uncurry sumEqualGoals) . pairs
  where
    sumEqualGoals :: Int -> Int -> Bool
    sumEqualGoals l r = l + r == target
    maybeProduct :: Int -> Int -> Maybe Int
    maybeProduct a b = Just (a * b)

pairs :: [a] -> [(a, a)]
pairs = map pair . combinations 2
  where
    pair :: [b] -> (b, b)
    pair [x, y] = (x, y)
    pair _ = error "this should never happen"

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys
    | y : xs' <- tails xs,
      ys <- combinations (n -1) xs'
  ]
