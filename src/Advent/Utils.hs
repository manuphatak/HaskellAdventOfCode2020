module Advent.Utils where

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

readInt :: String -> Int
readInt n = read n :: Int

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
