module Day10.Solution
  ( joltageJumps,
    parseAdapters,
    part1,
    part2,
    possibleArrangements,
  )
where

import Advent.Utils (parseInts)
import Data.Function ((&))
import Data.Sequence

part1 :: String -> String
part1 = show . (\(a, _, c) -> a * c) . joltageJumps . parseAdapters

part2 :: String -> String
part2 = show . possibleArrangements . parseAdapters

parseAdapters :: String -> Seq Int
parseAdapters = withLast . withZero . sort . fromList . parseInts
  where
    withZero :: Seq Int -> Seq Int
    withZero = (0 <|)
    withLast :: Seq Int -> Seq Int
    withLast (xs :|> x) = xs |> x |> x + 3
    withLast Empty = empty |> 3

joltageJumps :: Seq Int -> (Int, Int, Int)
joltageJumps = go (0, 0, 0)
  where
    go :: (Int, Int, Int) -> Seq Int -> (Int, Int, Int)
    go (a, b, c) (x :<| x' :<| xs) = go nextJumps (x' <| xs)
      where
        nextJumps = case x' - x of
          1 -> (succ a, b, c)
          2 -> (a, succ b, c)
          3 -> (a, b, succ c)
          _ -> error "Missing an adapter"
    go jumps _ = jumps

possibleArrangements :: Seq Int -> Int
possibleArrangements xs = countArrangements (xs, empty)

countArrangements :: (Seq Int, Seq (Int, Int)) -> Int
countArrangements (Empty, b :<| _) = snd b
countArrangements (Empty, Empty) = 0
countArrangements (0 :<| xs, bs) = countArrangements (xs, (0, 1) <| bs)
countArrangements (x :<| xs, bs) = countArrangements (xs, (x, cumulativePaths) <| bs)
  where
    cumulativePaths :: Int
    cumulativePaths =
      bs
        & takeWhileL (canReachCurrentAdapter . fst)
        & fmap snd
        & sum

    canReachCurrentAdapter :: Int -> Bool
    canReachCurrentAdapter b = x - b <= 3
