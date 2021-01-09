module Day23.Solution where

import Advent.Utils
import Day23.CircularList

part1 :: String -> String
part1 = cupOrder . moves 100 . parseCircularList

part2 :: String -> String
part2 = head . lines

parseCircularList :: String -> CircularList Int
parseCircularList = fromList . map (readInt . pure) . head . lines

move :: CircularList Int -> CircularList Int
move = putAtDestination . skipL 1 . yankR 3 . skipR 1
  where
    putAtDestination cl =
      let current = peek cl
          destination = (peek . skipL 1 . sort) cl
       in skipR 1 . skipWhileR (/= current) . putR . skipR 1 . skipWhileR (/= destination) $ cl

moves :: Int -> CircularList Int -> CircularList Int
moves n
  | n < 0 = undefined
  | n == 0 = id
  | otherwise = moves (pred n) . move

cupOrder :: CircularList Int -> String
cupOrder = concatMap show . tail . toList . skipWhileR (/= 1)
