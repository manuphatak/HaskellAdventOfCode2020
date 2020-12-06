module Day02.Utils where

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

infix 3 `xor`