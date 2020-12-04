module Day04.Solution (part1, part2, run, isValid) where

import Advent.Utils (occurrences)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences True . map isValid . fromRight [] . run

part2 :: String -> String
part2 = id

type Key = String

type Value = String

-- data Field = Field Key Value deriving (Show)

type Passport = Map.Map Key Value

passportsParser :: Parsec String st [Passport]
passportsParser = passportParser `sepBy` endOfLine

passportParser :: Parsec String st Passport
passportParser = Map.fromList <$> fieldParser `endBy` (space <|> endOfLine)

fieldParser :: Parsec String st (Key, Value)
fieldParser = (,) <$> keyParser <*> (char ':' *> valueParser)

valueParser :: Parsec String st Value
valueParser = many1 (alphaNum <|> char '#')

keyParser :: Parsec String st Key
keyParser = many1 alphaNum

run :: String -> Either ParseError [Passport]
run = parse passportsParser ""

isValid :: Passport -> Bool
isValid = and . sequenceA validations
  where
    validations =
      [ Map.member "byr",
        Map.member "iyr",
        Map.member "eyr",
        Map.member "hgt",
        Map.member "hcl",
        Map.member "ecl",
        Map.member "pid"
      ]

-- >>> input <- readFile "./test/Day04/example.txt"
-- >>> fmap (map isValid) . run $ input
-- Right [True,False,True,False]

-- Couldn't match type ‘[Passport]’ with ‘Map Key Value’
-- Expected type: String -> Either ParseError Passport
--   Actual type: String -> Either ParseError [Passport]

-- Right [
-- fromList [("byr","1937"),("cid","147"),("ecl","gry"),("eyr","2020"),("hcl","#fffffd"),("hgt","183cm"),("iyr","2017"),("pid","860033327")],
-- fromList [("byr","1929"),("cid","350"),("ecl","amb"),("eyr","2023"),("hcl","#cfa07d"),("iyr","2013"),("pid","028048884")],
-- fromList [("byr","1931"),("ecl","brn"),("eyr","2024"),("hcl","#ae17e1"),("hgt","179cm"),("iyr","2013"),("pid","760753108")],
-- fromList [("ecl","brn"),("eyr","2025"),("hcl","#cfa07d"),("hgt","59in"),("iyr","2011"),("pid","166559648")]]

-- ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd","byr:1937 iyr:2017 cid:147 hgt:183cm","","iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884","hcl:#cfa07d byr:1929","","hcl:#ae17e1 iyr:2013","eyr:2024","ecl:brn pid:760753108 byr:1931","hgt:179cm","","hcl:#cfa07d eyr:2025 pid:166559648","iyr:2011 ecl:brn hgt:59in"]

--  a -> [a -> Bool] -> Bool