module Day06.Solution (part1, part2, parseGroups, combineGroup, groupCounts) where

import Advent.Utils (rightToMaybe)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . sum . fromJust . groupCounts

part2 :: String -> String
part2 = id

type Passenger = String

type Group = [Passenger]

parseGroups :: String -> Either ParseError [Group]
parseGroups = parse (groupParser `sepEndBy1` newline) ""

groupParser :: Parsec String () Group
groupParser = passengerParser `sepEndBy1` newline

passengerParser :: Parsec String () Passenger
passengerParser = many1 letter

combineGroup :: Group -> String
combineGroup = Set.toList . Set.unions . map Set.fromList

groupCounts :: String -> Maybe [Int]
groupCounts = fmap (map (length . combineGroup)) . rightToMaybe . parseGroups

-- >>> input <- readFile "./test/Day06/example.txt"
-- >>> parseGroups input
-- Right [["abc"],["a","b","c"],["ab","ac"],["a","a","a","a"],["b"]]
