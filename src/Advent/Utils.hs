module Advent.Utils where

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower upper target = target >= lower && target <= upper

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

readInt :: String -> Int
readInt n = read n :: Int

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

fromLeft' :: Either a b -> a
fromLeft' (Right _) = error "Advent.Utils.fromLeft': Argument takes form 'Right _'"
fromLeft' (Left x) = x

-- http://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Combinators.html
fromRight' :: Either a b -> b
fromRight' (Left _) = error "Advent.Utils.fromRight': Argument takes form 'Left _'"
fromRight' (Right x) = x

fromRight'' :: Show a => Either a b -> b
fromRight'' (Left x) = error (show x)
fromRight'' (Right x) = x
