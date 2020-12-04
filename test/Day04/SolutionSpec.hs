module Day04.SolutionSpec (spec) where

import qualified Data.Map.Strict as Map
import Day04.Solution (isValid, part1, part2, run)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day04/input.txt"
    part1 input `shouldBe` "204"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day04/input.txt"
    part2 input `shouldBe` "hello santa"

  let expected =
        Right
          [ Map.fromList [("byr", "1937"), ("cid", "147"), ("ecl", "gry"), ("eyr", "2020"), ("hcl", "#fffffd"), ("hgt", "183cm"), ("iyr", "2017"), ("pid", "860033327")],
            Map.fromList [("byr", "1929"), ("cid", "350"), ("ecl", "amb"), ("eyr", "2023"), ("hcl", "#cfa07d"), ("iyr", "2013"), ("pid", "028048884")],
            Map.fromList [("byr", "1931"), ("ecl", "brn"), ("eyr", "2024"), ("hcl", "#ae17e1"), ("hgt", "179cm"), ("iyr", "2013"), ("pid", "760753108")],
            Map.fromList [("ecl", "brn"), ("eyr", "2025"), ("hcl", "#cfa07d"), ("hgt", "59in"), ("iyr", "2011"), ("pid", "166559648")]
          ]
  describe "runParser" $ do
    it "parses a list of Passports" $ do
      input <- readFile "./test/Day04/example.txt"
      run input `shouldBe` expected
  describe "isValid" $ do
    it "checks the validity of passports" $ do
      input <- readFile "./test/Day04/example.txt"
      (fmap (map isValid) . run $ input) `shouldBe` Right [True, False, True, False]
