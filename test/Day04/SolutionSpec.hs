module Day04.SolutionSpec (spec) where

import Advent.Utils (rightToMaybe)
import Data.Either (fromRight)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Day04.Solution (isValid, part1, part2, passportParser, run, strictFieldParser, strictPassportParser)
import Test.Hspec
import Text.Parsec (eof, parse)

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day04/input.txt"
    part1 input `shouldBe` "204"
  it "solves Part 2" $ do
    input <- readFile "./test/Day04/input.txt"
    part2 input `shouldBe` "179"

  describe "run" $ do
    describe "passportParser" $ do
      it "parses a list of Passports" $ do
        let expected = Right [Map.fromList [("byr", "1937"), ("cid", "147"), ("ecl", "gry"), ("eyr", "2020"), ("hcl", "#fffffd"), ("hgt", "183cm"), ("iyr", "2017"), ("pid", "860033327")], Map.fromList [("byr", "1929"), ("cid", "350"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#cfa07d"), ("iyr", "2013"), ("pid", "028048884")], Map.fromList [("byr", "1931"), ("ecl", "brn"), ("eyr", "2024"), ("hcl", "#ae17e1"), ("hgt", "179cm"), ("iyr", "2013"), ("pid", "760753108")], Map.fromList [("ecl", "brn"), ("eyr", "2025"), ("hcl", "#cfa07d"), ("hgt", "59in"), ("iyr", "2011"), ("pid", "166559648")]]
        input <- readFile "./test/Day04/example.txt"
        run passportParser input `shouldBe` expected

    describe "strictPassportParser" $ do
      it "parses a list of Passports" $ do
        let expected =
              Right
                [ Map.fromList [("byr", "1937"), ("cid", "147"), ("ecl", "gry"), ("eyr", "2020"), ("hcl", "#fffffd"), ("hgt", "183cm"), ("iyr", "2017"), ("pid", "860033327")],
                  Map.fromList [("byr", "1929"), ("cid", "350"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#cfa07d"), ("iyr", "2013"), ("pid", "028048884")],
                  Map.fromList [("byr", "1931"), ("ecl", "brn"), ("eyr", "2024"), ("hcl", "#ae17e1"), ("hgt", "179cm"), ("iyr", "2013"), ("pid", "760753108")],
                  Map.fromList [("ecl", "brn"), ("eyr", "2025"), ("hcl", "#cfa07d"), ("hgt", "59in"), ("iyr", "2011"), ("pid", "166559648")]
                ]
        input <- readFile "./test/Day04/example.txt"
        run strictPassportParser input `shouldBe` expected
    describe "isValid" $ do
      context "given the strictPassportParser" $ do
        it "parses a list of valid Passports" $ do
          let expected =
                [ Map.fromList [("byr", "1980"), ("ecl", "grn"), ("eyr", "2030"), ("hcl", "#623a2f"), ("hgt", "74in"), ("iyr", "2012"), ("pid", "087499704")],
                  Map.fromList [("byr", "1989"), ("cid", "129"), ("ecl", "blu"), ("eyr", "2029"), ("hcl", "#a97842"), ("hgt", "165cm"), ("iyr", "2014"), ("pid", "896056539")],
                  Map.fromList [("byr", "2001"), ("cid", "88"), ("ecl", "hzl"), ("eyr", "2022"), ("hcl", "#888785"), ("hgt", "164cm"), ("iyr", "2015"), ("pid", "545766238")],
                  Map.fromList [("byr", "1944"), ("ecl", "blu"), ("eyr", "2021"), ("hcl", "#b6652a"), ("hgt", "158cm"), ("iyr", "2010"), ("pid", "093154719")]
                ]
          input <- readFile "./test/Day04/strict-valid.txt"

          (filter isValid . fromRight [] . run strictPassportParser $ input) `shouldBe` expected
        it "parses a list of invalid Passwords" $ do
          input <- readFile "./test/Day04/strict-invalid.txt"

          (filter isValid . fromRight [] . run strictPassportParser $ input) `shouldBe` []

    context "given the passportParser" $ do
      it "checks the validity of passports" $ do
        input <- readFile "./test/Day04/example.txt"

        (fmap (map isValid) . run passportParser $ input) `shouldBe` Right [True, False, True, False]

  describe "strictFieldParser" $ do
    let cases =
          [ ("byr:1920", Just ("byr", "1920")),
            ("byr:2002", Just ("byr", "2002")),
            ("byr:1919", Nothing),
            ("byr:2003", Nothing),
            ("iyr:2010", Just ("iyr", "2010")),
            ("iyr:2020", Just ("iyr", "2020")),
            ("iyr:2009", Nothing),
            ("iyr:2021", Nothing),
            ("eyr:2020", Just ("eyr", "2020")),
            ("eyr:2030", Just ("eyr", "2030")),
            ("eyr:2019", Nothing),
            ("eyr:2031", Nothing),
            ("hgt:149cm", Nothing),
            ("hgt:150cm", Just ("hgt", "150cm")),
            ("hgt:193cm", Just ("hgt", "193cm")),
            ("hgt:194cm", Nothing),
            ("hgt:58in", Nothing),
            ("hgt:59in", Just ("hgt", "59in")),
            ("hgt:76in", Just ("hgt", "76in")),
            ("hgt:77in", Nothing),
            ("hgt:190in", Nothing),
            ("hgt:190", Nothing),
            ("hcl:#123abc", Just ("hcl", "#123abc")),
            ("hcl:#123abz", Nothing),
            ("hcl:123abc", Nothing),
            ("ecl:brn", Just ("ecl", "brn")),
            ("ecl:wat", Nothing),
            ("pid:000000001", Just ("pid", "000000001")),
            ("pid:0123456789", Nothing)
          ]
        test (input, expected) =
          it ("is " ++ show expected ++ " when parsing " ++ input) $
            (rightToMaybe $ parse (strictFieldParser <* eof) "" input) `shouldBe` expected
     in for_ cases test
