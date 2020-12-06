module Day06.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day06.Solution (combineGroup, groupCounts, parseGroups, part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day06/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day06/input.txt"
    part2 input `shouldBe` "hello santa"

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
  describe "combineGroup" $ do
    let cases =
          [ (["abc"], "abc"),
            (["a", "b", "c"], "abc"),
            (["ab", "ac"], "abc"),
            (["a", "a", "a", "a"], "a"),
            (["b"], "b")
          ]
        test (input, expected) = it ("is '" ++ expected ++ "' for group " ++ show input) $ do
          combineGroup input `shouldBe` expected
     in for_ cases test
  describe "groupCounts" $ do
    it "parses the example into groups of passenger" $ do
      input <- readFile "./test/Day06/example.txt"
      groupCounts input `shouldBe` Just [3, 3, 3, 1, 1]