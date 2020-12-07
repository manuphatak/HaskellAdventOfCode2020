module Day04.SolutionSpec (spec) where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Day04.Solution
  ( Validations (..),
    isValidLoose,
    isValidStrict,
    parsePassports,
    part1,
    part2,
    strictPassportValidations,
  )
import Test.Hspec hiding (example)

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day04/input.txt"
    part1 input `shouldBe` "204"
  it "solves Part 2" $ do
    input <- readFile "./test/Day04/input.txt"
    part2 input `shouldBe` "179"

  let example =
        [ Map.fromList [("byr", "1937"), ("cid", "147"), ("ecl", "gry"), ("eyr", "2020"), ("hcl", "#fffffd"), ("hgt", "183cm"), ("iyr", "2017"), ("pid", "860033327")],
          Map.fromList [("byr", "1929"), ("cid", "350"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#cfa07d"), ("iyr", "2013"), ("pid", "028048884")],
          Map.fromList [("byr", "1931"), ("ecl", "brn"), ("eyr", "2024"), ("hcl", "#ae17e1"), ("hgt", "179cm"), ("iyr", "2013"), ("pid", "760753108")],
          Map.fromList [("ecl", "brn"), ("eyr", "2025"), ("hcl", "#cfa07d"), ("hgt", "59in"), ("iyr", "2011"), ("pid", "166559648")]
        ]
  let strictValid =
        [ Map.fromList [("byr", "1980"), ("ecl", "grn"), ("eyr", "2030"), ("hcl", "#623a2f"), ("hgt", "74in"), ("iyr", "2012"), ("pid", "087499704")],
          Map.fromList [("byr", "1989"), ("cid", "129"), ("ecl", "blu"), ("eyr", "2029"), ("hcl", "#a97842"), ("hgt", "165cm"), ("iyr", "2014"), ("pid", "896056539")],
          Map.fromList [("byr", "2001"), ("cid", "88"), ("ecl", "hzl"), ("eyr", "2022"), ("hcl", "#888785"), ("hgt", "164cm"), ("iyr", "2015"), ("pid", "545766238")],
          Map.fromList [("byr", "1944"), ("ecl", "blu"), ("eyr", "2021"), ("hcl", "#b6652a"), ("hgt", "158cm"), ("iyr", "2010"), ("pid", "093154719")]
        ]
  let strictInvalid =
        [ Map.fromList [("byr", "1926"), ("cid", "100"), ("ecl", "amb"), ("eyr", "1972"), ("hcl", "#18171d"), ("hgt", "170"), ("iyr", "2018"), ("pid", "186cm")],
          Map.fromList [("byr", "1946"), ("ecl", "grn"), ("eyr", "1967"), ("hcl", "#602927"), ("hgt", "170cm"), ("iyr", "2019"), ("pid", "012533040")],
          Map.fromList [("byr", "1992"), ("cid", "277"), ("ecl", "brn"), ("eyr", "2020"), ("hcl", "dab227"), ("hgt", "182cm"), ("iyr", "2012"), ("pid", "021572410")],
          Map.fromList [("byr", "2007"), ("ecl", "zzz"), ("eyr", "2038"), ("hcl", "74454a"), ("hgt", "59cm"), ("iyr", "2023"), ("pid", "3556412378")]
        ]
  describe "parsePassports" $ do
    context "given the file example.txt" $
      it "parses a list of Passports" $ do
        input <- readFile "./test/Day04/example.txt"
        parsePassports input `shouldBe` Right example
    context "given the file strict-valid.txt" $
      it "parses a list of Passports" $ do
        input <- readFile "./test/Day04/strict-valid.txt"
        parsePassports input `shouldBe` Right strictValid
    context "given the file strict-invalid.txt" $
      it "parses a list of Passports" $ do
        input <- readFile "./test/Day04/strict-invalid.txt"
        parsePassports input `shouldBe` Right strictInvalid

  describe "isValidLoose" $
    context "given the example passports" $
      it "parses a list of Passports" $
        map isValidLoose example `shouldBe` [True, False, True, False]

  describe "isValidStrict" $
    context "given the strictValid passports" $
      it "parses a list of Passports" $
        map isValidStrict strictValid `shouldBe` [True, True, True, True]
  describe "isStrictInvalid" $
    context "given the strictValid passports" $
      it "parses a list of Passports" $
        map isValidStrict strictInvalid `shouldBe` [False, False, False, False]

  describe "strictPassportValidations" $
    let cases =
          [ ( "byr",
              [ ("1920", True),
                ("2002", True),
                ("1919", False),
                ("2003", False)
              ]
            ),
            ( "iyr",
              [ ("2010", True),
                ("2020", True),
                ("2009", False),
                ("2021", False)
              ]
            ),
            ( "eyr",
              [ ("2020", True),
                ("2030", True),
                ("2019", False),
                ("2031", False)
              ]
            ),
            ( "hgt",
              [ ("149cm", False),
                ("150cm", True),
                ("193cm", True),
                ("194cm", False),
                ("58in", False),
                ("59in", True),
                ("76in", True),
                ("77in", False),
                ("190in", False),
                ("190", False)
              ]
            ),
            ( "hcl",
              [ ("#123abc", True),
                ("#123abz", False),
                ("123abc", False)
              ]
            ),
            ( "ecl",
              [ ("brn", True),
                ("wat", False)
              ]
            ),
            ( "pid",
              [ ("000000001", True),
                ("0123456789", False)
              ]
            )
          ]
        test (key, contextCases) =
          context ("given key " ++ key) $ do
            let fn = case key of
                  "byr" -> byr
                  "iyr" -> iyr
                  "eyr" -> eyr
                  "hgt" -> hgt
                  "hcl" -> hcl
                  "ecl" -> ecl
                  "pid" -> pid
                  _ -> error ("unknown key " ++ key)

            let testContext (input, expected) =
                  it ("is " ++ show expected ++ " for " ++ input) $
                    fn strictPassportValidations (Just input) `shouldBe` expected

            for_ contextCases testContext
     in for_ cases test
