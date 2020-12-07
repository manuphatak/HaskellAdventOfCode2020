module Day04.Solution
  ( part1,
    part2,
    parsePassports,
    isValidLoose,
    isValidStrict,
    strictPassportValidations,
    Validations (..),
  )
where

import Advent.Utils (isBetween, readInt)
import Data.Either (fromRight, isRight)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Text.Parsec

part1 :: String -> String
part1 = show . length . filter isValidLoose . fromRight [] . parsePassports

part2 :: String -> String
part2 = show . length . filter isValidStrict . fromRight [] . parsePassports

type Key = String

type Value = String

type Passport = Map.Map Key Value

parsePassports :: String -> Either ParseError [Passport]
parsePassports = parse (passportParser `sepEndBy1` newline) ""

passportParser :: Parsec String st Passport
passportParser = Map.fromList <$> fieldParser `sepEndBy1` space

fieldParser :: Parsec String st (Key, Value)
fieldParser = (,) <$> keyParser <*> (char ':' *> valueParser)

valueParser :: Parsec String st Value
valueParser = many1 (alphaNum <|> char '#')

keyParser :: Parsec String st Key
keyParser = many1 alphaNum

isValidLoose :: Passport -> Bool
isValidLoose = isValidWith loosePassportValidations

isValidStrict :: Passport -> Bool
isValidStrict = isValidWith strictPassportValidations

type ValidateField = Maybe Value -> Bool

data Validations = Validations
  { byr :: ValidateField,
    iyr :: ValidateField,
    eyr :: ValidateField,
    hgt :: ValidateField,
    hcl :: ValidateField,
    ecl :: ValidateField,
    pid :: ValidateField
  }

isValidWith :: Validations -> Passport -> Bool
isValidWith validations =
  and
    . sequenceA
      [ byr validations . Map.lookup "byr",
        iyr validations . Map.lookup "iyr",
        eyr validations . Map.lookup "eyr",
        hgt validations . Map.lookup "hgt",
        hcl validations . Map.lookup "hcl",
        ecl validations . Map.lookup "ecl",
        pid validations . Map.lookup "pid"
      ]

loosePassportValidations :: Validations
loosePassportValidations =
  Validations
    { byr = isJust,
      iyr = isJust,
      eyr = isJust,
      hgt = isJust,
      hcl = isJust,
      ecl = isJust,
      pid = isJust
    }

strictPassportValidations :: Validations
strictPassportValidations =
  Validations
    { byr = maybe False (isBetween 1920 2002 . readInt),
      iyr = maybe False (isBetween 2010 2020 . readInt),
      eyr = maybe False (isBetween 2020 2030 . readInt),
      hgt = maybe False validHgt,
      hcl = maybe False validHcl,
      ecl = maybe False (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
      pid = maybe False validPid
    }
  where
    validHgt :: String -> Bool
    validHgt = isRight . parse (hgtParser <* eof) ""
    hgtParser :: Parsec String st String
    hgtParser = try (rangeParser 150 193 <* string "cm") <|> (rangeParser 59 76 <* string "in")

    validHcl :: String -> Bool
    validHcl = isRight . parse (char '#' *> count 6 hexDigit <* eof) ""

    validPid :: String -> Bool
    validPid = isRight . parse (count 9 digit <* eof) ""

rangeParser :: Int -> Int -> Parsec String st String
rangeParser lo hi = do
  value <- many1 digit

  if isBetween lo hi (readInt value)
    then return value
    else unexpected ("value " ++ value ++ ", expected a value between " ++ show lo ++ " and " ++ show hi)
