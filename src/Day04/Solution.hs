module Day04.Solution (part1, part2, run, isValid, passportParser, strictPassportParser, strictFieldParser) where

import Advent.Utils (isBetween, occurrences, readInt)
import Control.Monad (liftM2)
import Data.Either (fromRight)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences True . map isValid . fromRight [] . run passportParser

part2 :: String -> String
part2 = show . occurrences True . map isValidStrict . fromRight [] . run passportParser

type Key = String

type Value = String

type Passport = Map.Map Key Value

passportParser :: Parsec String st Passport
passportParser = Map.fromList <$> fieldParser `endBy1` (space <|> endOfLine)

strictPassportParser :: Parsec String st Passport
strictPassportParser = Map.fromList <$> try strictFieldParser `endBy1` (space <|> endOfLine)

fieldParser :: Parsec String st (Key, Value)
fieldParser = (,) <$> keyParser <*> (char ':' *> valueParser)

strictFieldParser :: Parsec String st (Key, Value)
strictFieldParser = do
  key <- keyParser
  _ <- char ':'
  value <- strictValueParser key
  return (key, value)

valueParser :: Parsec String st Value
valueParser = many1 (alphaNum <|> char '#')

strictValueParser :: Key -> Parsec String st Value
strictValueParser "byr" = yearRangeParser 1920 2002
strictValueParser "iyr" = yearRangeParser 2010 2020
strictValueParser "eyr" = yearRangeParser 2020 2030
strictValueParser "hgt" =
  choice
    [ try ((++) <$> rangeParser (many1 digit) 150 193 <*> string "cm"),
      try ((++) <$> rangeParser (many1 digit) 59 76 <*> string "in")
    ]
strictValueParser "hcl" = (:) <$> char '#' <*> count 6 hexDigit
strictValueParser "ecl" =
  choice
    [ try (string "amb"),
      try (string "blu"),
      try (string "brn"),
      try (string "gry"),
      try (string "grn"),
      try (string "hzl"),
      try (string "oth")
    ]
strictValueParser "pid" = count 9 digit
strictValueParser _ = many1 alphaNum

keyParser :: Parsec String st Key
keyParser = many1 alphaNum

yearRangeParser :: Int -> Int -> Parsec String st String
yearRangeParser = rangeParser (count 4 digit)

rangeParser :: Parsec String st String -> Int -> Int -> Parsec String st String
rangeParser parser lo hi =
  do
    value <- parser

    if isBetween lo hi (readInt value)
      then return value
      else unexpected ("value " ++ value ++ ", expected a value between " ++ show lo ++ " and " ++ show hi)

run :: Parsec String () Passport -> String -> Either ParseError [Passport]
run p = parse (p `sepBy1` endOfLine) ""

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

isValidStrict :: Passport -> Bool
isValidStrict = and . sequenceA validations
  where
    validations =
      [ isValid,
        isBetween 1920 2002 . readInt . fromJust . Map.lookup "byr",
        isBetween 2010 2020 . readInt . fromJust . Map.lookup "iyr",
        isBetween 2020 2030 . readInt . fromJust . Map.lookup "eyr",
        validHgt . fromJust . Map.lookup "hgt",
        validHcl . fromJust . Map.lookup "hcl",
        (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) . fromJust . Map.lookup "ecl",
        validPid . fromJust . Map.lookup "pid"
      ]
    validHgt :: String -> Bool
    validHgt value
      | "cm" `isSuffixOf` value = isBetween 150 193 . readInt $ take (length value - 2) value
      | "in" `isSuffixOf` value = isBetween 59 76 . readInt $ take (length value - 2) value
      | otherwise = False
    validHcl :: String -> Bool

    validHcl value
      | head value == '#' = length (tail value) == 6 && all (`elem` "abcdef0123456789") (tail value)
      | otherwise = False
    validPid :: String -> Bool
    validPid value = liftM2 (&&) ((== 9) . length) (all (`elem` "0123456789")) $ value

-- >>> input <- readFile "./test/Day04/example.txt"
-- >>> parse strictFieldParser "" "eyr:2031"
-- Left (line 1, column 9):
-- unexpected value 2031, expected a value between 2020 and 2030

-- >>> input <- readFile "./test/Day04/input.txt"
-- >>> filter isValid . fromRight [] . run passportParser $ input

_loose :: [Passport]
_loose =
  [ Map.fromList [("byr", "1939"), ("ecl", "blu"), ("eyr", "2029"), ("hcl", "#ceb3a1"), ("hgt", "163cm"), ("iyr", "2013"), ("pid", "660456119")],
    Map.fromList [("byr", "1975"), ("cid", "207"), ("ecl", "grn"), ("eyr", "2028"), ("hcl", "#0f8b2e"), ("hgt", "158cm"), ("iyr", "2011"), ("pid", "755567813")],
    Map.fromList [("byr", "2002"), ("cid", "293"), ("ecl", "brn"), ("eyr", "2021"), ("hcl", "#18171d"), ("hgt", "177cm"), ("iyr", "2012"), ("pid", "867936514")],
    Map.fromList [("byr", "1953"), ("ecl", "grn"), ("eyr", "2021"), ("hcl", "#733820"), ("hgt", "193cm"), ("iyr", "2010"), ("pid", "214278931")],
    Map.fromList [("byr", "1934"), ("ecl", "oth"), ("eyr", "2020"), ("hcl", "#866857"), ("hgt", "161cm"), ("iyr", "2010"), ("pid", "022785900")],
    Map.fromList [("byr", "1928"), ("cid", "262"), ("ecl", "brn"), ("eyr", "2021"), ("hcl", "#602927"), ("hgt", "166cm"), ("iyr", "2010"), ("pid", "393738288")],
    Map.fromList [("byr", "2001"), ("cid", "317"), ("ecl", "grn"), ("eyr", "2023"), ("hcl", "#6b5442"), ("hgt", "177cm"), ("iyr", "2016"), ("pid", "407685013")],
    Map.fromList [("byr", "1921"), ("cid", "59"), ("ecl", "grn"), ("eyr", "2022"), ("hcl", "#86127d"), ("hgt", "180cm"), ("iyr", "2018"), ("pid", "113577635")],
    Map.fromList [("byr", "1984"), ("cid", "177"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#fffffd"), ("hgt", "152cm"), ("iyr", "2015"), ("pid", "379600323")],
    Map.fromList [("byr", "1974"), ("cid", "262"), ("ecl", "hzl"), ("eyr", "2020"), ("hcl", "#efcc98"), ("hgt", "159cm"), ("iyr", "2016"), ("pid", "520627197")],
    Map.fromList [("byr", "1979"), ("cid", "302"), ("ecl", "gry"), ("eyr", "1956"), ("hcl", "#3355a2"), ("hgt", "158cm"), ("iyr", "2015"), ("pid", "282247859")],
    Map.fromList [("byr", "1938"), ("ecl", "amb"), ("eyr", "2025"), ("hcl", "#733820"), ("hgt", "187cm"), ("iyr", "2020"), ("pid", "156126542")],
    Map.fromList [("byr", "1952"), ("cid", "230"), ("ecl", "blu"), ("eyr", "2023"), ("hcl", "#341e13"), ("hgt", "59in"), ("iyr", "2015"), ("pid", "955240866")],
    Map.fromList [("byr", "1925"), ("ecl", "grn"), ("eyr", "2020"), ("hcl", "#341e13"), ("hgt", "186cm"), ("iyr", "2013"), ("pid", "783635907")],
    Map.fromList [("byr", "1921"), ("ecl", "oth"), ("eyr", "2027"), ("hcl", "#18171d"), ("hgt", "175cm"), ("iyr", "2019"), ("pid", "355931973")],
    Map.fromList [("byr", "1947"), ("cid", "160"), ("ecl", "hzl"), ("eyr", "1976"), ("hcl", "z"), ("hgt", "146"), ("iyr", "2014"), ("pid", "164cm")],
    Map.fromList [("byr", "1985"), ("ecl", "gry"), ("eyr", "2023"), ("hcl", "#c0946f"), ("hgt", "170cm"), ("iyr", "2014"), ("pid", "686203227")],
    Map.fromList [("byr", "1954"), ("ecl", "oth"), ("eyr", "2022"), ("hcl", "#c0946f"), ("hgt", "192cm"), ("iyr", "2016"), ("pid", "371575068")],
    Map.fromList [("byr", "1922"), ("cid", "236"), ("ecl", "brn"), ("eyr", "2023"), ("hcl", "#ceb3a1"), ("hgt", "72in"), ("iyr", "2013"), ("pid", "877594317")],
    Map.fromList [("byr", "1944"), ("ecl", "blu"), ("eyr", "2028"), ("hcl", "#602927"), ("hgt", "178cm"), ("iyr", "2019"), ("pid", "277638047")],
    Map.fromList [("byr", "1979"), ("cid", "91"), ("ecl", "hzl"), ("eyr", "2022"), ("hcl", "#a97842"), ("hgt", "164cm"), ("iyr", "2019"), ("pid", "371132831")],
    Map.fromList [("byr", "1993"), ("cid", "346"), ("ecl", "hzl"), ("eyr", "2030"), ("hcl", "#341e13"), ("hgt", "178cm"), ("iyr", "2019"), ("pid", "335209743")],
    Map.fromList [("byr", "1980"), ("ecl", "lzr"), ("eyr", "2039"), ("hcl", "z"), ("hgt", "65cm"), ("iyr", "2024"), ("pid", "96101128")],
    Map.fromList [("byr", "1956"), ("ecl", "grn"), ("eyr", "2027"), ("hcl", "#888785"), ("hgt", "174cm"), ("iyr", "2017"), ("pid", "565437685")],
    Map.fromList [("byr", "1947"), ("cid", "77"), ("ecl", "gry"), ("eyr", "2024"), ("hcl", "#602927"), ("hgt", "164cm"), ("iyr", "2017"), ("pid", "769106108")],
    Map.fromList [("byr", "1993"), ("cid", "212"), ("ecl", "gry"), ("eyr", "2022"), ("hcl", "#fffffd"), ("hgt", "187cm"), ("iyr", "2019"), ("pid", "475618502")],
    Map.fromList [("byr", "1971"), ("ecl", "grn"), ("eyr", "2022"), ("hcl", "#623a2f"), ("hgt", "191cm"), ("iyr", "2018"), ("pid", "166515049")],
    Map.fromList [("byr", "2029"), ("ecl", "#4ddfb1"), ("eyr", "2029"), ("hcl", "2912ec"), ("hgt", "150cm"), ("iyr", "2011"), ("pid", "15469000")],
    Map.fromList [("byr", "1941"), ("ecl", "grn"), ("eyr", "2024"), ("hcl", "#888785"), ("hgt", "166cm"), ("iyr", "2010"), ("pid", "273390626")],
    Map.fromList [("byr", "1957"), ("cid", "301"), ("ecl", "amb"), ("eyr", "2030"), ("hcl", "#18171d"), ("hgt", "156cm"), ("iyr", "2015"), ("pid", "677032916")],
    Map.fromList [("byr", "1967"), ("cid", "128"), ("ecl", "amb"), ("eyr", "2021"), ("hcl", "#c83b73"), ("hgt", "162cm"), ("iyr", "2010"), ("pid", "104999760")],
    Map.fromList [("byr", "1930"), ("cid", "173"), ("ecl", "gry"), ("eyr", "2028"), ("hcl", "#7d3b0c"), ("hgt", "161cm"), ("iyr", "2013"), ("pid", "810244270")],
    Map.fromList [("byr", "1975"), ("ecl", "blu"), ("eyr", "2028"), ("hcl", "#cfa07d"), ("hgt", "184cm"), ("iyr", "2019"), ("pid", "7243957480")],
    Map.fromList [("byr", "1953"), ("ecl", "blu"), ("eyr", "2020"), ("hcl", "#ceb3a1"), ("hgt", "157cm"), ("iyr", "2017"), ("pid", "807621409")],
    Map.fromList [("byr", "1965"), ("ecl", "blu"), ("eyr", "2027"), ("hcl", "#a97842"), ("hgt", "179cm"), ("iyr", "2014"), ("pid", "896621814")],
    Map.fromList [("byr", "1963"), ("ecl", "oth"), ("eyr", "2024"), ("hcl", "#ceb3a1"), ("hgt", "188cm"), ("iyr", "2015"), ("pid", "827722366")],
    Map.fromList [("byr", "1959"), ("cid", "215"), ("ecl", "oth"), ("eyr", "2027"), ("hcl", "#602927"), ("hgt", "150cm"), ("iyr", "2014"), ("pid", "948589059")],
    Map.fromList [("byr", "1979"), ("ecl", "hzl"), ("eyr", "2027"), ("hcl", "#873cdf"), ("hgt", "188cm"), ("iyr", "2019"), ("pid", "738493109")],
    Map.fromList [("byr", "1978"), ("ecl", "oth"), ("eyr", "2025"), ("hcl", "#18171d"), ("hgt", "164cm"), ("iyr", "2012"), ("pid", "563787480")],
    Map.fromList [("byr", "1971"), ("ecl", "blu"), ("eyr", "2028"), ("hcl", "#6b5442"), ("hgt", "190cm"), ("iyr", "2012"), ("pid", "758307028")],
    Map.fromList [("byr", "1997"), ("cid", "186"), ("ecl", "amb"), ("eyr", "2027"), ("hcl", "#c0946f"), ("hgt", "165cm"), ("iyr", "2019"), ("pid", "904275084")],
    Map.fromList [("byr", "1932"), ("cid", "77"), ("ecl", "brn"), ("eyr", "2027"), ("hcl", "#602927"), ("hgt", "158cm"), ("iyr", "2010"), ("pid", "382971064")],
    Map.fromList [("byr", "1956"), ("ecl", "grn"), ("eyr", "2028"), ("hcl", "#fffffd"), ("hgt", "156cm"), ("iyr", "2020"), ("pid", "249987568")],
    Map.fromList [("byr", "1947"), ("ecl", "blu"), ("eyr", "2023"), ("hcl", "#b62955"), ("hgt", "192cm"), ("iyr", "2012"), ("pid", "190618020")],
    Map.fromList [("byr", "1996"), ("ecl", "gry"), ("eyr", "2022"), ("hcl", "#f0a94b"), ("hgt", "191cm"), ("iyr", "2020"), ("pid", "699194379")],
    Map.fromList [("byr", "1971"), ("ecl", "brn"), ("eyr", "2020"), ("hcl", "498a57"), ("hgt", "69in"), ("iyr", "2020"), ("pid", "368841807")],
    Map.fromList [("byr", "1934"), ("ecl", "gry"), ("eyr", "2028"), ("hcl", "#cfa07d"), ("hgt", "156cm"), ("iyr", "2012"), ("pid", "034993671")],
    Map.fromList [("byr", "1937"), ("ecl", "blu"), ("eyr", "2028"), ("hcl", "#fffffd"), ("hgt", "65in"), ("iyr", "2013"), ("pid", "777520867")],
    Map.fromList [("byr", "1965"), ("ecl", "gry"), ("eyr", "2023"), ("hcl", "#c0946f"), ("hgt", "185cm"), ("iyr", "2012"), ("pid", "289861622")],
    Map.fromList [("byr", "1977"), ("cid", "309"), ("ecl", "hzl"), ("eyr", "2023"), ("hcl", "#cfa07d"), ("hgt", "62in"), ("iyr", "2012"), ("pid", "447717379")],
    Map.fromList [("byr", "1934"), ("cid", "294"), ("ecl", "amb"), ("eyr", "2025"), ("hcl", "#efcc98"), ("hgt", "178cm"), ("iyr", "2012"), ("pid", "385150170")],
    Map.fromList [("byr", "1983"), ("cid", "107"), ("ecl", "gry"), ("eyr", "2029"), ("hcl", "#341e13"), ("hgt", "160cm"), ("iyr", "2015"), ("pid", "396708902")],
    Map.fromList [("byr", "1932"), ("ecl", "grn"), ("eyr", "2022"), ("hcl", "#a97842"), ("hgt", "174cm"), ("iyr", "2014"), ("pid", "304883665")],
    Map.fromList [("byr", "2002"), ("cid", "245"), ("ecl", "amb"), ("eyr", "2022"), ("hcl", "#ceb3a1"), ("hgt", "157cm"), ("iyr", "2017"), ("pid", "142383109")],
    Map.fromList [("byr", "1986"), ("ecl", "grn"), ("eyr", "2023"), ("hcl", "#c0946f"), ("hgt", "71in"), ("iyr", "2012"), ("pid", "108222056")],
    Map.fromList [("byr", "1955"), ("ecl", "blu"), ("eyr", "2020"), ("hcl", "#a97842"), ("hgt", "175cm"), ("iyr", "2018"), ("pid", "164459538")],
    Map.fromList [("byr", "1950"), ("cid", "273"), ("ecl", "grn"), ("eyr", "2025"), ("hcl", "#341e13"), ("hgt", "177cm"), ("iyr", "2010"), ("pid", "473932418")],
    Map.fromList [("byr", "1967"), ("cid", "178"), ("ecl", "amb"), ("eyr", "2027"), ("hcl", "#866857"), ("hgt", "187cm"), ("iyr", "2014"), ("pid", "366720897")],
    Map.fromList [("byr", "1938"), ("cid", "181"), ("ecl", "amb"), ("eyr", "2024"), ("hcl", "#341e13"), ("hgt", "174cm"), ("iyr", "2018"), ("pid", "478505944")],
    Map.fromList [("byr", "1938"), ("ecl", "brn"), ("eyr", "2025"), ("hcl", "z"), ("hgt", "180cm"), ("iyr", "2013"), ("pid", "178969784")],
    Map.fromList [("byr", "1950"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#733820"), ("hgt", "71in"), ("iyr", "2016"), ("pid", "447434977")],
    Map.fromList [("byr", "1945"), ("cid", "173"), ("ecl", "oth"), ("eyr", "2025"), ("hcl", "#a97842"), ("hgt", "163cm"), ("iyr", "2010"), ("pid", "418871498")],
    Map.fromList [("byr", "1937"), ("ecl", "hzl"), ("eyr", "2026"), ("hcl", "#623a2f"), ("hgt", "163cm"), ("iyr", "2018"), ("pid", "017817627")],
    Map.fromList [("byr", "1982"), ("cid", "123"), ("ecl", "hzl"), ("eyr", "2021"), ("hcl", "#efcc98"), ("hgt", "162cm"), ("iyr", "2012"), ("pid", "099838535")],
    Map.fromList [("byr", "1969"), ("ecl", "brn"), ("eyr", "2020"), ("hcl", "#6b5442"), ("hgt", "184cm"), ("iyr", "2013"), ("pid", "041801541")],
    Map.fromList [("byr", "1934"), ("ecl", "oth"), ("eyr", "2023"), ("hcl", "#fffffd"), ("hgt", "151cm"), ("iyr", "2019"), ("pid", "314689468")],
    Map.fromList [("byr", "1953"), ("cid", "193"), ("ecl", "oth"), ("eyr", "2022"), ("hcl", "#a87847"), ("hgt", "179cm"), ("iyr", "2016"), ("pid", "966876004")],
    Map.fromList [("byr", "1955"), ("cid", "111"), ("ecl", "blu"), ("eyr", "2028"), ("hcl", "#602927"), ("hgt", "179cm"), ("iyr", "2019"), ("pid", "659491942")],
    Map.fromList [("byr", "1994"), ("cid", "83"), ("ecl", "oth"), ("eyr", "2030"), ("hcl", "#602927"), ("hgt", "180cm"), ("iyr", "2011"), ("pid", "160688997")],
    Map.fromList [("byr", "1950"), ("ecl", "brn"), ("eyr", "2025"), ("hcl", "#130738"), ("hgt", "167cm"), ("iyr", "2012"), ("pid", "686738997")],
    Map.fromList [("byr", "1937"), ("cid", "309"), ("ecl", "hzl"), ("eyr", "2026"), ("hcl", "#c0946f"), ("hgt", "155cm"), ("iyr", "2015"), ("pid", "208013639")],
    Map.fromList [("byr", "1949"), ("ecl", "amb"), ("eyr", "2022"), ("hcl", "#18171d"), ("hgt", "191cm"), ("iyr", "2015"), ("pid", "181247866")],
    Map.fromList [("byr", "1969"), ("cid", "171"), ("ecl", "amb"), ("eyr", "2020"), ("hcl", "#7d3b0c"), ("hgt", "193cm"), ("iyr", "2012"), ("pid", "770110016")],
    Map.fromList [("byr", "1937"), ("ecl", "blu"), ("eyr", "2020"), ("hcl", "#866857"), ("hgt", "162cm"), ("iyr", "2020"), ("pid", "057170032")],
    Map.fromList [("byr", "1962"), ("cid", "233"), ("ecl", "hzl"), ("eyr", "2027"), ("hcl", "#623a2f"), ("hgt", "176cm"), ("iyr", "2018"), ("pid", "516772675")],
    Map.fromList [("byr", "1981"), ("ecl", "blu"), ("eyr", "2025"), ("hcl", "#a97842"), ("hgt", "65in"), ("iyr", "2011"), ("pid", "722094416")],
    Map.fromList [("byr", "1953"), ("ecl", "gry"), ("eyr", "2027"), ("hcl", "#866857"), ("hgt", "192cm"), ("iyr", "2010"), ("pid", "532811620")],
    Map.fromList [("byr", "1983"), ("ecl", "grn"), ("eyr", "2021"), ("hcl", "#7d3b0c"), ("hgt", "161cm"), ("iyr", "2017"), ("pid", "242124004")],
    Map.fromList [("byr", "1945"), ("ecl", "gry"), ("eyr", "2030"), ("hcl", "#efcc98"), ("hgt", "162cm"), ("iyr", "2013"), ("pid", "032502598")],
    Map.fromList [("byr", "1981"), ("ecl", "hzl"), ("eyr", "2023"), ("hcl", "#888785"), ("hgt", "165cm"), ("iyr", "2020"), ("pid", "851962288")],
    Map.fromList [("byr", "1932"), ("ecl", "brn"), ("eyr", "2030"), ("hcl", "#623a2f"), ("hgt", "176cm"), ("iyr", "2014"), ("pid", "676358652")],
    Map.fromList [("byr", "2029"), ("ecl", "#698113"), ("eyr", "2034"), ("hcl", "z"), ("hgt", "76cm"), ("iyr", "1963"), ("pid", "174cm")],
    Map.fromList [("byr", "1948"), ("cid", "204"), ("ecl", "oth"), ("eyr", "2028"), ("hcl", "#c0946f"), ("hgt", "156cm"), ("iyr", "2019"), ("pid", "229984095")],
    Map.fromList [("byr", "1992"), ("ecl", "amb"), ("eyr", "2029"), ("hcl", "#efcc98"), ("hgt", "188cm"), ("iyr", "2010"), ("pid", "859107750")],
    Map.fromList [("byr", "1966"), ("cid", "261"), ("ecl", "gry"), ("eyr", "2027"), ("hcl", "#602927"), ("hgt", "164cm"), ("iyr", "2011"), ("pid", "754939860")],
    Map.fromList [("byr", "1991"), ("ecl", "brn"), ("eyr", "2027"), ("hcl", "#602927"), ("hgt", "71in"), ("iyr", "2019"), ("pid", "368543014")],
    Map.fromList [("byr", "1934"), ("cid", "66"), ("ecl", "grt"), ("eyr", "2028"), ("hcl", "#341e13"), ("hgt", "188cm"), ("iyr", "2014"), ("pid", "410490656")],
    Map.fromList [("byr", "1932"), ("ecl", "amb"), ("eyr", "2020"), ("hcl", "#a6304b"), ("hgt", "188cm"), ("iyr", "1947"), ("pid", "29447176")],
    Map.fromList [("byr", "1994"), ("ecl", "grn"), ("eyr", "2029"), ("hcl", "#6b5442"), ("hgt", "155cm"), ("iyr", "2010"), ("pid", "978117883")],
    Map.fromList [("byr", "1958"), ("ecl", "grn"), ("eyr", "2029"), ("hcl", "#7d3b0c"), ("hgt", "182cm"), ("iyr", "2019"), ("pid", "466450124")],
    Map.fromList [("byr", "1970"), ("cid", "247"), ("ecl", "blu"), ("eyr", "2021"), ("hcl", "#cfa07d"), ("hgt", "172cm"), ("iyr", "2014"), ("pid", "810234340")],
    Map.fromList [("byr", "1946"), ("cid", "88"), ("ecl", "gry"), ("eyr", "2029"), ("hcl", "#7d3b0c"), ("hgt", "170cm"), ("iyr", "2015"), ("pid", "579575823")],
    Map.fromList [("byr", "1936"), ("cid", "272"), ("ecl", "grn"), ("eyr", "2020"), ("hcl", "#6b5442"), ("hgt", "190cm"), ("iyr", "2015"), ("pid", "752699834")],
    Map.fromList [("byr", "1996"), ("cid", "85"), ("ecl", "hzl"), ("eyr", "2025"), ("hcl", "#602927"), ("hgt", "192cm"), ("iyr", "2018"), ("pid", "478812793")],
    Map.fromList [("byr", "1968"), ("ecl", "hzl"), ("eyr", "2027"), ("hcl", "#9b96f3"), ("hgt", "160cm"), ("iyr", "2016"), ("pid", "054732103")],
    Map.fromList [("byr", "1931"), ("cid", "346"), ("ecl", "oth"), ("eyr", "2022"), ("hcl", "#efcc98"), ("hgt", "152cm"), ("iyr", "2010"), ("pid", "140347821")],
    Map.fromList [("byr", "1963"), ("ecl", "amb"), ("eyr", "2021"), ("hcl", "#b6652a"), ("hgt", "190cm"), ("iyr", "2015"), ("pid", "158cm")],
    Map.fromList [("byr", "1981"), ("cid", "144"), ("ecl", "brn"), ("eyr", "2028"), ("hcl", "#c0946f"), ("hgt", "157cm"), ("iyr", "2013"), ("pid", "384713923")],
    Map.fromList [("byr", "1981"), ("ecl", "gry"), ("eyr", "2027"), ("hcl", "#006f93"), ("hgt", "150cm"), ("iyr", "2015"), ("pid", "769266043")],
    Map.fromList [("byr", "1993"), ("ecl", "amb"), ("eyr", "2022"), ("hcl", "#341e13"), ("hgt", "154cm"), ("iyr", "2020"), ("pid", "516617153")],
    Map.fromList [("byr", "1996"), ("cid", "178"), ("ecl", "brn"), ("eyr", "2022"), ("hcl", "#7d3b0c"), ("hgt", "70in"), ("iyr", "2010"), ("pid", "205918254")],
    Map.fromList [("byr", "2001"), ("ecl", "brn"), ("eyr", "2024"), ("hcl", "#888785"), ("hgt", "186cm"), ("iyr", "2019"), ("pid", "218031016")],
    Map.fromList [("byr", "1971"), ("ecl", "oth"), ("eyr", "2026"), ("hcl", "#a97842"), ("hgt", "152cm"), ("iyr", "2017"), ("pid", "673909751")],
    Map.fromList [("byr", "1966"), ("cid", "334"), ("ecl", "brn"), ("eyr", "2023"), ("hcl", "#ceb3a1"), ("hgt", "188cm"), ("iyr", "2017"), ("pid", "#8a11cd")],
    Map.fromList [("byr", "1989"), ("ecl", "amb"), ("eyr", "2026"), ("hcl", "#b6652a"), ("hgt", "154cm"), ("iyr", "2019"), ("pid", "835997489")],
    Map.fromList [("byr", "1934"), ("ecl", "blu"), ("eyr", "2030"), ("hcl", "#fffffd"), ("hgt", "68in"), ("iyr", "2010"), ("pid", "741394760")],
    Map.fromList [("byr", "1981"), ("cid", "307"), ("ecl", "brn"), ("eyr", "2029"), ("hcl", "#602927"), ("hgt", "154cm"), ("iyr", "2019"), ("pid", "329288264")],
    Map.fromList [("byr", "1968"), ("ecl", "brn"), ("eyr", "2029"), ("hcl", "#866857"), ("hgt", "152cm"), ("iyr", "2016"), ("pid", "347204193")],
    Map.fromList [("byr", "1927"), ("ecl", "hzl"), ("eyr", "2020"), ("hcl", "#efcc98"), ("hgt", "166cm"), ("iyr", "2015"), ("pid", "975616547")],
    Map.fromList [("byr", "1938"), ("ecl", "gry"), ("eyr", "2021"), ("hcl", "#18171d"), ("hgt", "160cm"), ("iyr", "2020"), ("pid", "938987284")],
    Map.fromList [("byr", "1967"), ("cid", "154"), ("ecl", "amb"), ("eyr", "2026"), ("hcl", "#888785"), ("hgt", "166cm"), ("iyr", "2011"), ("pid", "727980371")],
    Map.fromList [("byr", "1928"), ("ecl", "grn"), ("eyr", "2024"), ("hcl", "#888785"), ("hgt", "150cm"), ("iyr", "2018"), ("pid", "852102448")],
    Map.fromList [("byr", "1997"), ("cid", "201"), ("ecl", "gry"), ("eyr", "2020"), ("hcl", "#7d3b0c"), ("hgt", "163cm"), ("iyr", "2011"), ("pid", "052314445")],
    Map.fromList [("byr", "1965"), ("ecl", "hzl"), ("eyr", "2024"), ("hcl", "#c0946f"), ("hgt", "189cm"), ("iyr", "2015"), ("pid", "460808964")],
    Map.fromList [("byr", "1944"), ("cid", "163"), ("ecl", "grn"), ("eyr", "2027"), ("hcl", "#efcc98"), ("hgt", "159cm"), ("iyr", "2010"), ("pid", "731085710")],
    Map.fromList [("byr", "1964"), ("ecl", "gry"), ("eyr", "2023"), ("hcl", "526fbd"), ("hgt", "71cm"), ("iyr", "2016"), ("pid", "981371510")],
    Map.fromList [("byr", "1987"), ("ecl", "gry"), ("eyr", "2028"), ("hcl", "#18171d"), ("hgt", "168cm"), ("iyr", "2014"), ("pid", "264437557")],
    Map.fromList [("byr", "2001"), ("ecl", "hzl"), ("eyr", "2025"), ("hcl", "#2b965f"), ("hgt", "154cm"), ("iyr", "2011"), ("pid", "574867413")],
    Map.fromList [("byr", "1959"), ("cid", "249"), ("ecl", "brn"), ("eyr", "2040"), ("hcl", "#b6652a"), ("hgt", "177cm"), ("iyr", "2011"), ("pid", "447524365")],
    Map.fromList [("byr", "1935"), ("cid", "128"), ("ecl", "amb"), ("eyr", "2027"), ("hcl", "#fffffd"), ("hgt", "60in"), ("iyr", "2016"), ("pid", "270076583")],
    Map.fromList [("byr", "1935"), ("cid", "332"), ("ecl", "blu"), ("eyr", "2025"), ("hcl", "#c0946f"), ("hgt", "185cm"), ("iyr", "2016"), ("pid", "149533201")],
    Map.fromList [("byr", "1928"), ("cid", "275"), ("ecl", "grn"), ("eyr", "2022"), ("hcl", "#f27d9b"), ("hgt", "59in"), ("iyr", "2017"), ("pid", "311342224")],
    Map.fromList [("byr", "1985"), ("cid", "131"), ("ecl", "gry"), ("eyr", "2029"), ("hcl", "#6b5442"), ("hgt", "191cm"), ("iyr", "2018"), ("pid", "957166785")],
    Map.fromList [("byr", "1982"), ("ecl", "blu"), ("eyr", "2020"), ("hcl", "#623a2f"), ("hgt", "192cm"), ("iyr", "2012"), ("pid", "741921163")],
    Map.fromList [("byr", "1995"), ("ecl", "gry"), ("eyr", "2027"), ("hcl", "#ceb3a1"), ("hgt", "164cm"), ("iyr", "2017"), ("pid", "086846266")],
    Map.fromList [("byr", "1966"), ("cid", "107"), ("ecl", "brn"), ("eyr", "2020"), ("hcl", "#c0946f"), ("hgt", "158cm"), ("iyr", "2014"), ("pid", "548013549")],
    Map.fromList [("byr", "1942"), ("cid", "104"), ("ecl", "grn"), ("eyr", "2021"), ("hcl", "#888785"), ("hgt", "192cm"), ("iyr", "2015"), ("pid", "582902279")],
    Map.fromList [("byr", "1923"), ("cid", "153"), ("ecl", "amb"), ("eyr", "2030"), ("hcl", "#fffffd"), ("hgt", "185cm"), ("iyr", "2020"), ("pid", "216803187")],
    Map.fromList [("byr", "1964"), ("cid", "336"), ("ecl", "gry"), ("eyr", "2020"), ("hcl", "#733820"), ("hgt", "156cm"), ("iyr", "2011"), ("pid", "129687562")],
    Map.fromList [("byr", "1924"), ("cid", "155"), ("ecl", "oth"), ("eyr", "2024"), ("hcl", "#866857"), ("hgt", "156cm"), ("iyr", "2012"), ("pid", "814749853")],
    Map.fromList [("byr", "1949"), ("cid", "188"), ("ecl", "oth"), ("eyr", "2022"), ("hcl", "#adeffb"), ("hgt", "151cm"), ("iyr", "2010"), ("pid", "832407555")],
    Map.fromList [("byr", "1999"), ("cid", "102"), ("ecl", "grn"), ("eyr", "2029"), ("hcl", "#a97842"), ("hgt", "173cm"), ("iyr", "2013"), ("pid", "199221595")],
    Map.fromList [("byr", "1963"), ("cid", "230"), ("ecl", "amb"), ("eyr", "2029"), ("hcl", "#fffffd"), ("hgt", "171cm"), ("iyr", "2010"), ("pid", "980136208")],
    Map.fromList [("byr", "1944"), ("ecl", "amb"), ("eyr", "2022"), ("hcl", "#13682c"), ("hgt", "184cm"), ("iyr", "2016"), ("pid", "764280754")],
    Map.fromList [("byr", "2008"), ("ecl", "utc"), ("eyr", "2026"), ("hcl", "#888785"), ("hgt", "152cm"), ("iyr", "1921"), ("pid", "993871206")],
    Map.fromList [("byr", "1960"), ("cid", "287"), ("ecl", "amb"), ("eyr", "2021"), ("hcl", "#888785"), ("hgt", "179cm"), ("iyr", "2016"), ("pid", "043166927")],
    Map.fromList [("byr", "2013"), ("ecl", "oth"), ("eyr", "2026"), ("hcl", "#fffffd"), ("hgt", "183cm"), ("iyr", "2020"), ("pid", "042844334")],
    Map.fromList [("byr", "1995"), ("ecl", "hzl"), ("eyr", "2025"), ("hcl", "#c0946f"), ("hgt", "162cm"), ("iyr", "2011"), ("pid", "36857936")],
    Map.fromList [("byr", "1946"), ("cid", "92"), ("ecl", "blu"), ("eyr", "2027"), ("hcl", "#7d3b0c"), ("hgt", "180cm"), ("iyr", "2013"), ("pid", "150720364")],
    Map.fromList [("byr", "1992"), ("cid", "276"), ("ecl", "amb"), ("eyr", "2020"), ("hcl", "#6b5442"), ("hgt", "59in"), ("iyr", "2011"), ("pid", "137604720")],
    Map.fromList [("byr", "1987"), ("cid", "176"), ("ecl", "oth"), ("eyr", "2024"), ("hcl", "#623a2f"), ("hgt", "69in"), ("iyr", "2015"), ("pid", "872650041")],
    Map.fromList [("byr", "1970"), ("ecl", "amb"), ("eyr", "2024"), ("hcl", "#ceb3a1"), ("hgt", "175cm"), ("iyr", "2019"), ("pid", "388875093")],
    Map.fromList [("byr", "1999"), ("cid", "186"), ("ecl", "blu"), ("eyr", "2022"), ("hcl", "#7d3b0c"), ("hgt", "160cm"), ("iyr", "2019"), ("pid", "040506316")],
    Map.fromList [("byr", "1981"), ("cid", "98"), ("ecl", "brn"), ("eyr", "2029"), ("hcl", "#7d3b0c"), ("hgt", "163cm"), ("iyr", "2016"), ("pid", "500276094")],
    Map.fromList [("byr", "1938"), ("ecl", "oth"), ("eyr", "2023"), ("hcl", "#c0946f"), ("hgt", "158cm"), ("iyr", "1966"), ("pid", "544377825")],
    Map.fromList [("byr", "1975"), ("cid", "57"), ("ecl", "oth"), ("eyr", "2022"), ("hcl", "#cfa07d"), ("hgt", "180cm"), ("iyr", "2016"), ("pid", "144977701")],
    Map.fromList [("byr", "1955"), ("ecl", "amb"), ("eyr", "2020"), ("hcl", "#a97842"), ("hgt", "156cm"), ("iyr", "2011"), ("pid", "591688826")],
    Map.fromList [("byr", "1927"), ("ecl", "blu"), ("eyr", "2024"), ("hcl", "#a97842"), ("hgt", "174cm"), ("iyr", "2012"), ("pid", "928395919")],
    Map.fromList [("byr", "1997"), ("ecl", "amb"), ("eyr", "2021"), ("hcl", "#888785"), ("hgt", "156"), ("iyr", "2019"), ("pid", "980577052")],
    Map.fromList [("byr", "1930"), ("ecl", "oth"), ("eyr", "2028"), ("hcl", "#602927"), ("hgt", "177cm"), ("iyr", "2014"), ("pid", "846909255")],
    Map.fromList [("byr", "1941"), ("cid", "54"), ("ecl", "gry"), ("eyr", "2026"), ("hcl", "#efcc98"), ("hgt", "166cm"), ("iyr", "2015"), ("pid", "387565513")],
    Map.fromList [("byr", "1963"), ("ecl", "brn"), ("eyr", "2024"), ("hcl", "#fffffd"), ("hgt", "178cm"), ("iyr", "2014"), ("pid", "241522887")],
    Map.fromList [("byr", "1964"), ("cid", "77"), ("ecl", "grn"), ("eyr", "2026"), ("hcl", "#c0946f"), ("hgt", "70in"), ("iyr", "2018"), ("pid", "202158837")],
    Map.fromList [("byr", "1939"), ("ecl", "grn"), ("eyr", "2025"), ("hcl", "#fffffd"), ("hgt", "176cm"), ("iyr", "2016"), ("pid", "927969731")],
    Map.fromList [("byr", "1943"), ("ecl", "hzl"), ("eyr", "2020"), ("hcl", "#c2a152"), ("hgt", "191cm"), ("iyr", "2017"), ("pid", "422579553")],
    Map.fromList [("byr", "1924"), ("ecl", "brn"), ("eyr", "2030"), ("hcl", "#341e13"), ("hgt", "157cm"), ("iyr", "2014"), ("pid", "823049712")],
    Map.fromList [("byr", "1954"), ("cid", "194"), ("ecl", "hzl"), ("eyr", "2030"), ("hcl", "#ceb3a1"), ("hgt", "183cm"), ("iyr", "2013"), ("pid", "146052548")],
    Map.fromList [("byr", "1920"), ("ecl", "hzl"), ("eyr", "2026"), ("hcl", "#6b5442"), ("hgt", "65in"), ("iyr", "2014"), ("pid", "694135829")],
    Map.fromList [("byr", "1939"), ("ecl", "grn"), ("eyr", "2026"), ("hcl", "#a97842"), ("hgt", "183cm"), ("iyr", "2014"), ("pid", "853148268")],
    Map.fromList [("byr", "1922"), ("cid", "165"), ("ecl", "hzl"), ("eyr", "2021"), ("hcl", "#b6652a"), ("hgt", "182cm"), ("iyr", "2016"), ("pid", "199346431")],
    Map.fromList [("byr", "1947"), ("ecl", "hzl"), ("eyr", "2030"), ("hcl", "#341e13"), ("hgt", "164cm"), ("iyr", "2015"), ("pid", "466649892")],
    Map.fromList [("byr", "1921"), ("ecl", "brn"), ("eyr", "2029"), ("hcl", "#a97842"), ("hgt", "191cm"), ("iyr", "2016"), ("pid", "776471818")],
    Map.fromList [("byr", "1991"), ("ecl", "amb"), ("eyr", "2026"), ("hcl", "#7d3b0c"), ("hgt", "158cm"), ("iyr", "2014"), ("pid", "605101404")],
    Map.fromList [("byr", "1995"), ("cid", "271"), ("ecl", "hzl"), ("eyr", "2029"), ("hcl", "#18171d"), ("hgt", "165cm"), ("iyr", "2012"), ("pid", "723865532")],
    Map.fromList [("byr", "2023"), ("ecl", "hzl"), ("eyr", "2025"), ("hcl", "#7d3b0c"), ("hgt", "65cm"), ("iyr", "2010"), ("pid", "83552498")],
    Map.fromList [("byr", "1975"), ("cid", "166"), ("ecl", "brn"), ("eyr", "2022"), ("hcl", "#888785"), ("hgt", "165cm"), ("iyr", "2013"), ("pid", "261135534")],
    Map.fromList [("byr", "1954"), ("cid", "273"), ("ecl", "#915d48"), ("eyr", "2021"), ("hcl", "#cfa07d"), ("hgt", "167cm"), ("iyr", "2018"), ("pid", "004261180")],
    Map.fromList [("byr", "1931"), ("cid", "130"), ("ecl", "gry"), ("eyr", "2029"), ("hcl", "#733820"), ("hgt", "183cm"), ("iyr", "2015"), ("pid", "962443962")],
    Map.fromList [("byr", "1959"), ("ecl", "amb"), ("eyr", "2027"), ("hcl", "#602927"), ("hgt", "155cm"), ("iyr", "2020"), ("pid", "293590926")],
    Map.fromList [("byr", "1929"), ("ecl", "oth"), ("eyr", "2026"), ("hcl", "#fffffd"), ("hgt", "181cm"), ("iyr", "2020"), ("pid", "469517546")],
    Map.fromList [("byr", "1944"), ("cid", "254"), ("ecl", "grn"), ("eyr", "2026"), ("hcl", "#341e13"), ("hgt", "64cm"), ("iyr", "2015"), ("pid", "644263519")],
    Map.fromList [("byr", "1922"), ("ecl", "oth"), ("eyr", "2028"), ("hcl", "#341e13"), ("hgt", "150cm"), ("iyr", "2017"), ("pid", "618759433")],
    Map.fromList [("byr", "1987"), ("cid", "221"), ("ecl", "gry"), ("eyr", "2030"), ("hcl", "#efcc98"), ("hgt", "158cm"), ("iyr", "2011"), ("pid", "897252903")],
    Map.fromList [("byr", "2028"), ("cid", "148"), ("ecl", "grt"), ("eyr", "2036"), ("hcl", "df9a7d"), ("hgt", "101"), ("iyr", "1957"), ("pid", "#185456")],
    Map.fromList [("byr", "1922"), ("cid", "316"), ("ecl", "gry"), ("eyr", "2023"), ("hcl", "#b6652a"), ("hgt", "150cm"), ("iyr", "2013"), ("pid", "858150885")],
    Map.fromList [("byr", "1996"), ("ecl", "oth"), ("eyr", "2024"), ("hcl", "#602927"), ("hgt", "179cm"), ("iyr", "2019"), ("pid", "539623368")],
    Map.fromList [("byr", "1986"), ("ecl", "grn"), ("eyr", "2029"), ("hcl", "#18171d"), ("hgt", "189cm"), ("iyr", "2012"), ("pid", "499804970")],
    Map.fromList [("byr", "1990"), ("ecl", "blu"), ("eyr", "2026"), ("hcl", "#623a2f"), ("hgt", "179cm"), ("iyr", "2011"), ("pid", "306676089")],
    Map.fromList [("byr", "1987"), ("ecl", "oth"), ("eyr", "2022"), ("hcl", "#5a9d3e"), ("hgt", "68in"), ("iyr", "2013"), ("pid", "894642127")],
    Map.fromList [("byr", "1953"), ("ecl", "oth"), ("eyr", "2023"), ("hcl", "#7d3b0c"), ("hgt", "157cm"), ("iyr", "2011"), ("pid", "059006259")],
    Map.fromList [("byr", "1959"), ("cid", "130"), ("ecl", "oth"), ("eyr", "2021"), ("hcl", "#b6652a"), ("hgt", "75in"), ("iyr", "2020"), ("pid", "255405447")],
    Map.fromList [("byr", "1963"), ("cid", "124"), ("ecl", "oth"), ("eyr", "2020"), ("hcl", "#7d3b0c"), ("hgt", "66in"), ("iyr", "2020"), ("pid", "353288807")],
    Map.fromList [("byr", "1960"), ("cid", "256"), ("ecl", "hzl"), ("eyr", "2026"), ("hcl", "#3a4c6e"), ("hgt", "150cm"), ("iyr", "2015"), ("pid", "027677435")],
    Map.fromList [("byr", "1939"), ("cid", "170"), ("ecl", "brn"), ("eyr", "2028"), ("hcl", "#7d3b0c"), ("hgt", "175cm"), ("iyr", "2020"), ("pid", "620159472")],
    Map.fromList [("byr", "1938"), ("cid", "152"), ("ecl", "brn"), ("eyr", "2030"), ("hcl", "#fffffd"), ("hgt", "161cm"), ("iyr", "2019"), ("pid", "311625365")],
    Map.fromList [("byr", "1934"), ("cid", "235"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#efcc98"), ("hgt", "174cm"), ("iyr", "2018"), ("pid", "389966928")],
    Map.fromList [("byr", "1960"), ("ecl", "grn"), ("eyr", "2022"), ("hcl", "#623a2f"), ("hgt", "152cm"), ("iyr", "2010"), ("pid", "556639427")],
    Map.fromList [("byr", "1946"), ("cid", "152"), ("ecl", "oth"), ("eyr", "2028"), ("hcl", "#623a2f"), ("hgt", "62in"), ("iyr", "2016"), ("pid", "007367220")],
    Map.fromList [("byr", "1987"), ("ecl", "oth"), ("eyr", "2028"), ("hcl", "#2cc1fd"), ("hgt", "163cm"), ("iyr", "2013"), ("pid", "346162920")],
    Map.fromList [("byr", "1952"), ("cid", "290"), ("ecl", "oth"), ("eyr", "2029"), ("hcl", "#fffffd"), ("hgt", "63in"), ("iyr", "2013"), ("pid", "636103282")],
    Map.fromList [("byr", "1951"), ("ecl", "amb"), ("eyr", "2021"), ("hcl", "#866857"), ("hgt", "157cm"), ("iyr", "2013"), ("pid", "258369047")],
    Map.fromList [("byr", "1983"), ("ecl", "hzl"), ("eyr", "2029"), ("hcl", "#623a2f"), ("hgt", "182cm"), ("iyr", "2026"), ("pid", "373940630")],
    Map.fromList [("byr", "1954"), ("cid", "215"), ("ecl", "amb"), ("eyr", "2025"), ("hcl", "#ceb3a1"), ("hgt", "164cm"), ("iyr", "2015"), ("pid", "459309210")],
    Map.fromList [("byr", "1931"), ("cid", "293"), ("ecl", "oth"), ("eyr", "2028"), ("hcl", "#6b5442"), ("hgt", "177cm"), ("iyr", "2015"), ("pid", "494763488")],
    Map.fromList [("byr", "1953"), ("cid", "330"), ("ecl", "oth"), ("eyr", "2024"), ("hcl", "#888785"), ("hgt", "153cm"), ("iyr", "2014"), ("pid", "532786321")],
    Map.fromList [("byr", "1958"), ("ecl", "grn"), ("eyr", "2023"), ("hcl", "#b6652a"), ("hgt", "180cm"), ("iyr", "2012"), ("pid", "441199844")],
    Map.fromList [("byr", "1928"), ("cid", "176"), ("ecl", "brn"), ("eyr", "2028"), ("hcl", "#341e13"), ("hgt", "174cm"), ("iyr", "2011"), ("pid", "835022632")],
    Map.fromList [("byr", "1974"), ("cid", "178"), ("ecl", "hzl"), ("eyr", "2023"), ("hcl", "#6b5442"), ("hgt", "168cm"), ("iyr", "2020"), ("pid", "067761346")],
    Map.fromList [("byr", "1963"), ("ecl", "gry"), ("eyr", "2025"), ("hcl", "#ceb3a1"), ("hgt", "157cm"), ("iyr", "2011"), ("pid", "803912278")],
    Map.fromList [("byr", "1974"), ("ecl", "blu"), ("eyr", "2021"), ("hcl", "#733820"), ("hgt", "174in"), ("iyr", "2020"), ("pid", "228596339")],
    Map.fromList [("byr", "1953"), ("cid", "99"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#118493"), ("hgt", "165cm"), ("iyr", "2019"), ("pid", "458049702")],
    Map.fromList [("byr", "1938"), ("cid", "261"), ("ecl", "amb"), ("eyr", "2021"), ("hcl", "#a97842"), ("hgt", "61in"), ("iyr", "2016"), ("pid", "162402242")],
    Map.fromList [("byr", "1980"), ("ecl", "brn"), ("eyr", "2027"), ("hcl", "#ceb3a1"), ("hgt", "187cm"), ("iyr", "2016"), ("pid", "977322718")],
    Map.fromList [("byr", "1965"), ("ecl", "oth"), ("eyr", "2027"), ("hcl", "#6b5442"), ("hgt", "153cm"), ("iyr", "2010"), ("pid", "455361219")]
  ]

_strict :: [Passport]
_strict =
  [ Map.fromList [("byr", "1939"), ("ecl", "blu"), ("eyr", "2029"), ("hcl", "#ceb3a1"), ("hgt", "163cm"), ("iyr", "2013"), ("pid", "660456119")],
    Map.fromList [("byr", "1975"), ("cid", "207"), ("ecl", "grn"), ("eyr", "2028"), ("hcl", "#0f8b2e"), ("hgt", "158cm"), ("iyr", "2011"), ("pid", "755567813")],
    Map.fromList [("byr", "2002"), ("cid", "293"), ("ecl", "brn"), ("eyr", "2021"), ("hcl", "#18171d"), ("hgt", "177cm"), ("iyr", "2012"), ("pid", "867936514")],
    Map.fromList [("byr", "1953"), ("ecl", "grn"), ("eyr", "2021"), ("hcl", "#733820"), ("hgt", "193cm"), ("iyr", "2010"), ("pid", "214278931")],
    Map.fromList [("byr", "1934"), ("ecl", "oth"), ("eyr", "2020"), ("hcl", "#866857"), ("hgt", "161cm"), ("iyr", "2010"), ("pid", "022785900")],
    Map.fromList [("byr", "1928"), ("cid", "262"), ("ecl", "brn"), ("eyr", "2021"), ("hcl", "#602927"), ("hgt", "166cm"), ("iyr", "2010"), ("pid", "393738288")],
    Map.fromList [("byr", "2001"), ("cid", "317"), ("ecl", "grn"), ("eyr", "2023"), ("hcl", "#6b5442"), ("hgt", "177cm"), ("iyr", "2016"), ("pid", "407685013")],
    Map.fromList [("byr", "1921"), ("cid", "59"), ("ecl", "grn"), ("eyr", "2022"), ("hcl", "#86127d"), ("hgt", "180cm"), ("iyr", "2018"), ("pid", "113577635")],
    Map.fromList [("byr", "1984"), ("cid", "177"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#fffffd"), ("hgt", "152cm"), ("iyr", "2015"), ("pid", "379600323")],
    Map.fromList [("byr", "1930"), ("cid", "101"), ("ecl", "amb"), ("eyr", "2024"), ("hcl", "#fffffd"), ("hgt", "154cm"), ("pid", "919013176")],
    Map.fromList [("byr", "1920"), ("cid", "337"), ("ecl", "gry"), ("eyr", "2026"), ("hcl", "#a97842"), ("hgt", "76in"), ("pid", "612193949")],
    Map.fromList [("byr", "1974"), ("cid", "262"), ("ecl", "hzl"), ("eyr", "2020"), ("hcl", "#efcc98"), ("hgt", "159cm"), ("iyr", "2016"), ("pid", "520627197")],
    Map.fromList [("cid", "302")]
  ]
