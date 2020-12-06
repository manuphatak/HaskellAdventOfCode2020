module Advent.Utils where

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
