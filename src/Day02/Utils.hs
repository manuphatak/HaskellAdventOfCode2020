module Day02.Utils where

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

infix 3 `xor`