module Advent.Utils where

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)
