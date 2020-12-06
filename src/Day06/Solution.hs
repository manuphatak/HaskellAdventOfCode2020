module Day06.Solution (part1, part2, parseGroups, anyYes, allYes, groupCounts) where

import Advent.Utils (rightToMaybe)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . sum . fromJust . groupCounts anyYes

part2 :: String -> String
part2 = show . sum . fromJust . groupCounts allYes

type Passenger = String

type Group = [Passenger]

parseGroups :: String -> Either ParseError [Group]
parseGroups = parse (groupParser `sepEndBy1` newline) ""

groupParser :: Parsec String () Group
groupParser = passengerParser `sepEndBy1` newline

passengerParser :: Parsec String () Passenger
passengerParser = many1 letter

anyYes :: Group -> String
anyYes = Set.toList . Set.unions . map Set.fromList

allYes :: Group -> String
allYes group = filter (\char -> all (elem char) group) $ anyYes group

groupCounts :: (Group -> String) -> String -> Maybe [Int]
groupCounts combineGroup = fmap (map (length . combineGroup)) . rightToMaybe . parseGroups
