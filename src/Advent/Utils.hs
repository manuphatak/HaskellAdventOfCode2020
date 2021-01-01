{-# LANGUAGE TypeApplications #-}

module Advent.Utils where

import Data.List (tails)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

occurrences :: (Foldable t, Eq a) => a -> t a -> Int
occurrences target = foldr go 0
  where
    go value
      | value == target = succ
      | otherwise = id

readInt :: String -> Int
readInt = read @Int

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

fromRightOrShowError :: Show a => Either a b -> b
fromRightOrShowError (Left x) = error (show x)
fromRightOrShowError (Right x) = x
