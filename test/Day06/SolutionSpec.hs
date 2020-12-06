module Day06.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day06.Solution (allYes, anyYes, groupCounts, parseGroups, part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day06/input.txt"
    part1 input `shouldBe` "6735"
  it "solves Part 2" $ do
    input <- readFile "./test/Day06/input.txt"
    part2 input `shouldBe` "3221"

  describe "parseGroups" $ do
    it "parses the example into groups of passenger" $ do
      input <- readFile "./test/Day06/example.txt"
      parseGroups input
        `shouldBe` Right
          [ ["abc"],
            ["a", "b", "c"],
            ["ab", "ac"],
            ["a", "a", "a", "a"],
            ["b"]
          ]
  describe "anyYes" $ do
    let cases =
          [ (["abc"], "abc"),
            (["a", "b", "c"], "abc"),
            (["ab", "ac"], "abc"),
            (["a", "a", "a", "a"], "a"),
            (["b"], "b")
          ]
        test (input, expected) = it ("finds '" ++ expected ++ "' are answered 'yes' for ANY passengers within " ++ show input) $ do
          anyYes input `shouldBe` expected
     in for_ cases test
  describe "allYes" $ do
    let cases =
          [ (["abc"], "abc"),
            (["a", "b", "c"], ""),
            (["ab", "ac"], "a"),
            (["a", "a", "a", "a"], "a"),
            (["b"], "b")
          ]
        test (input, expected) = it ("finds '" ++ expected ++ "' are answered 'yes' for ALL passengers within " ++ show input) $ do
          allYes input `shouldBe` expected
     in for_ cases test
  describe "groupCounts" $ do
    context "when counting ANY 'yes's within a group" $ do
      it "finds the count for each group" $ do
        input <- readFile "./test/Day06/example.txt"
        groupCounts anyYes input `shouldBe` Just [3, 3, 3, 1, 1]
    context "when counting ALL 'yes's within a group" $ do
      it "finds the count for each group" $ do
        input <- readFile "./test/Day06/example.txt"
        groupCounts allYes input `shouldBe` Just [3, 0, 1, 1, 1]
