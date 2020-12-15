module Day13.Solution where

import Advent.Utils
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe
import Text.Parsec

part1 :: String -> String
part1 = show . uncurry (*) . earliestBus . fromRightOrShowError . parseSchedule

part2 :: String -> String
part2 = show . busAlignment . snd . fromRightOrShowError . parseSchedule

parseSchedule :: String -> Either ParseError (Int, [Maybe Int])
parseSchedule = parse scheduleParser ""
  where
    scheduleParser :: Parsec String () (Int, [Maybe Int])
    scheduleParser = (,) <$> (intParser <* endOfLine) <*> busIdsParser

parseBusIds :: String -> Either ParseError [Maybe Int]
parseBusIds = parse busIdsParser ""

busIdsParser :: Parsec String () [Maybe Int]
busIdsParser = busIdParser `sepBy1` char ','
  where
    busIdParser :: Parsec String () (Maybe Int)
    busIdParser = choice [Nothing <$ try (char 'x'), Just <$> intParser]

intParser :: Parsec String () Int
intParser = read <$> many digit

earliestBus :: (Int, [Maybe Int]) -> (Int, Int)
earliestBus (start, busIds) = minimumBy (compare `on` fst) . map nextDeparture . catMaybes $ busIds
  where
    nextDeparture :: Int -> (Int, Int)
    nextDeparture bus = (bus - (start `mod` bus), bus)

busAlignment :: [Maybe Int] -> Int
busAlignment busIds = go (nextBus - offset)
  where
    busses :: [(Int, Int)]
    busses = sortBy (flip compare `on` snd) . map (second fromJust) . filter (isJust . snd) . zip [0 ..] $ busIds

    (offset, nextBus) = head busses

    maximumIterations = 9999999999

    -- maximumIterations = 999999999999999
    -- maximumIterations = 100000000000000

    go :: Int -> Int
    go t
      | t > maximumIterations = error "timestamp is greater than maximumIterations"
      | all (match t) busses = t
      | otherwise = go (t + nextBus)

    match :: Int -> (Int, Int) -> Bool
    match t (i, bus) = ((t + i) `mod` bus) == 0
