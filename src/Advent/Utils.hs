module Advent.Utils where

import Data.List (tails)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

readInt :: String -> Int
readInt n = read n :: Int

parseInts :: String -> [Int]
parseInts = map readInt . lines

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ y : ys
    | y : xs' <- tails xs,
      ys <- combinations (pred n) xs'
  ]

fromRightOrError' :: Show a => Either a b -> b
fromRightOrError' (Left x) = error (show x)
fromRightOrError' (Right x) = x
