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

wolframAlphaQuery :: String -> String
wolframAlphaQuery = intercalate ", " . map (uncurry asQuery) . foldr catSndMaybes [] . zip [0 ..] . snd . fromRightOrShowError . parseSchedule
  where
    catSndMaybes :: (Int, Maybe Int) -> [(Int, Int)] -> [(Int, Int)]
    catSndMaybes (_, Nothing) xs = xs
    catSndMaybes (i, Just bus) xs = (i, bus) : xs

    -- ((x + index) mod bus) == 0
    asQuery :: Int -> Int -> String
    asQuery index bus = "(x + " ++ show index ++ ") mod " ++ show bus

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
