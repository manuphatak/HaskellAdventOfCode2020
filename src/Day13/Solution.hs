module Day13.Solution
  ( busAlignment,
    earliestBus,
    parseBusIds,
    parseSchedule,
    part1,
    part2,
    wolframAlphaQuery,
  )
where

import Advent.Utils (fromRightOrShowError)
import Data.Bifunctor (Bifunctor (second))
import Data.Function (on)
import Data.List (intercalate, minimumBy, sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)
import Text.Parsec

part1 :: String -> String
part1 = show . uncurry (*) . earliestBus . fromRightOrShowError . parseSchedule

part2 :: String -> String
part2 = show . busAlignment . snd . fromRightOrShowError . parseSchedule

wolframAlphaQuery :: String -> String
wolframAlphaQuery =
  intercalate ", "
    . foldr asQuery []
    . zip [0 ..]
    . snd
    . fromRightOrShowError
    . parseSchedule
  where
    asQuery :: (Int, Maybe Int) -> [String] -> [String]
    asQuery (_, Nothing) xs = xs
    asQuery (index, Just bus) xs = ("(x + " ++ show index ++ ") mod " ++ show bus) : xs

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
    (offset, nextBus) : busses =
      sortBy
        (flip compare `on` snd)
        . map (second fromJust)
        . filter (isJust . snd)
        . zip [0 ..]
        $ busIds

    go :: Int -> Int
    go t
      | all (match t) busses = t
      | otherwise = go (t + nextBus)

    match :: Int -> (Int, Int) -> Bool
    match t (i, bus) = ((t + i) `mod` bus) == 0
