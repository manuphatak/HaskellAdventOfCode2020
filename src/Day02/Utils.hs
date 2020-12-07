module Day02.Utils where

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

infix 3 `xor`
