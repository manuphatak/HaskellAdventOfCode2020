module Day13.Solution
  ( busAlignment,
    crt,
    earliestBus,
    parseBusIds,
    parseSchedule,
    part1,
    part2,
  )
where

import Advent.Utils (fromRightOrShowError)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Text.Parsec

part1 :: String -> String
part1 = show . uncurry (*) . earliestBus . fromRightOrShowError . parseSchedule

part2 :: String -> String
part2 = show . busAlignment . snd . fromRightOrShowError . parseSchedule

parseSchedule :: String -> Either ParseError (Integer, [Maybe Integer])
parseSchedule = parse scheduleParser ""
  where
    scheduleParser :: Parsec String () (Integer, [Maybe Integer])
    scheduleParser = (,) <$> (intParser <* endOfLine) <*> busIdsParser

parseBusIds :: String -> Either ParseError [Maybe Integer]
parseBusIds = parse busIdsParser ""

busIdsParser :: Parsec String () [Maybe Integer]
busIdsParser = busIdParser `sepBy1` char ','
  where
    busIdParser :: Parsec String () (Maybe Integer)
    busIdParser = choice [Nothing <$ try (char 'x'), Just <$> intParser]

intParser :: Parsec String () Integer
intParser = read <$> many digit

earliestBus :: (Integer, [Maybe Integer]) -> (Integer, Integer)
earliestBus (start, busIds) = minimumBy (compare `on` fst) . map nextDeparture . catMaybes $ busIds
  where
    nextDeparture :: Integer -> (Integer, Integer)
    nextDeparture bus = (bus - (start `mod` bus), bus)

busAlignment :: [Maybe Integer] -> Integer
busAlignment = uncurry subtract . crt . enumeratedBusIds
  where
    enumeratedBusIds :: [Maybe Integer] -> [(Integer, Integer)]
    enumeratedBusIds xs = [(a, b) | (a, Just b) <- zip [0 ..] xs]

-- Chinese Remainder Theorem
-- https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go :: Integral a => (a, a) -> (a, a) -> (a, a)
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    inv :: Integral a => a -> a -> a
    a `inv` m = let (_, i, _) = gcd' a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd' :: Integral c => c -> c -> (c, c, c)
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd' (b `mod` a) a
