module Advent.Utils where

import Data.List (tails)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

occurrences :: (Foldable t, Num b, Eq a) => a -> t a -> b
occurrences target = foldr fn 0
  where
    fn value = if value == target then (+ 1) else (+ 0)

readInt :: String -> Int
readInt n = read n :: Int

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
