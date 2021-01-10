module Day23.Solution where

import Advent.Utils (readInt)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.IntMap.Lazy as IntMap

part1 :: String -> String
part1 = cupOrder . moves 100 . fromList . parseInts

part2 :: String -> String
part2 = show . product . adjacentTo 1 . moves (10 * oneMillion) . fillCups . parseInts

oneMillion :: Int
oneMillion = 1000000

type CircularList = (Int, IntMap.IntMap Int)

moves :: Int -> CircularList -> CircularList
moves n
  | n < 0 = undefined
  | n == 0 = id
  | otherwise = moves (pred n) . move

move :: CircularList -> CircularList
move xss@(pointer, xs) =
  ( d,
    (IntMap.insert c c' . IntMap.insert destination a . IntMap.insert pointer d) xs
  )
  where
    [a, b, c, d] = take 4 . drop 1 . toList $ xss
    c' = xs ! destination
    destination = findDestination (pred pointer)

    findDestination candidate
      | candidate < 0 = undefined
      | candidate == 0 = (findDestination . maximum . IntMap.keys) xs
      | candidate `elem` [a, b, c] = findDestination (pred candidate)
      | otherwise = candidate

fromList :: [Int] -> CircularList
fromList xss@(x : xs) =
  ( x,
    IntMap.fromList (zip xss (xs ++ [x]))
  )
fromList _ = undefined

toUniqList :: CircularList -> [Int]
toUniqList = takeWhileUniq . toList
  where
    takeWhileUniq :: Eq a => [a] -> [a]
    takeWhileUniq = foldr go []
      where
        go x r = x : takeWhile (/= x) r

toList :: CircularList -> [Int]
toList = map fst . iterate go
  where
    go :: CircularList -> CircularList
    go (pointer, cl) = (cl ! pointer, cl)

(!) :: IntMap.IntMap Int -> Int -> Int
m ! k = IntMap.findWithDefault (succ k) k m

parseInts :: String -> [Int]
parseInts = map (readInt . pure) . head . lines

cupOrder :: CircularList -> String
cupOrder = concatMap show . tail . toUniqList . goTo 1

fillCups :: [Int] -> CircularList
fillCups xs =
  ( pointer,
    (IntMap.insert oneMillion pointer . IntMap.insert lastKey (succ maxKey)) xs'
  )
  where
    (pointer, xs') = fromList xs
    maxKey = maximum xs
    lastKey = last xs

goTo :: Int -> CircularList -> CircularList
goTo n = first (const n)

adjacentTo :: Int -> CircularList -> [Int]
adjacentTo n = take 2 . drop 1 . toList . goTo n
