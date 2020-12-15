module Day13.Solution where

import Advent.Utils
import Data.Function
import Data.List
import Data.Maybe
import Text.Parsec

part1 :: String -> String
part1 = show . uncurry (*) . earliestBus . fromRightOrShowError . parseSchedule

part2 :: String -> String
part2 = head . lines

parseSchedule :: String -> Either ParseError (Int, [Maybe Int])
parseSchedule = parse scheduleParser ""
  where
    scheduleParser :: Parsec String () (Int, [Maybe Int])
    scheduleParser = (,) <$> (intParser <* endOfLine) <*> (busIdParser `sepBy1` char ',')

    intParser :: Parsec String () Int
    intParser = read <$> many digit

    busIdParser :: Parsec String () (Maybe Int)
    busIdParser = choice [Nothing <$ try (char 'x'), Just <$> intParser]

earliestBus :: (Int, [Maybe Int]) -> (Int, Int)
earliestBus (start, busIds) = minimumBy (compare `on` fst) . map nextDeparture . catMaybes $ busIds
  where
    nextDeparture :: Int -> (Int, Int)
    nextDeparture bus = (bus - (start `mod` bus), bus)
