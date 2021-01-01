module Day09.SolutionSpec (spec) where

import Advent.Utils (parseInts)
import Day09.Solution (encryptionWeakness, part1, part2, rollingChunks', xmasCypher)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day09/input.txt"
    part1 input `shouldBe` "167829540"
  it "solves Part 2" $ do
    input <- readFile "./test/Day09/input.txt"
    part2 input `shouldBe` "28045630"
  describe "xmasCypher" $ do
    it "finds the first number that does not follow the preamble" $ do
      input <- readFile "./test/Day09/example.txt"
      xmasCypher 5 input `shouldBe` Just 127
  describe "encryptionWeakness" $ do
    it "finds the contiguous set of numbers that sum to the target" $ do
      input <- parseInts <$> readFile "./test/Day09/example.txt"
      encryptionWeakness 127 input `shouldBe` 62
  describe "rollingChunks'" $ do
    it "iterates through a list for case 1" $ do
      rollingChunks' 2 "abcd" `shouldBe` [("ab", 'c'), ("bc", 'd')]
    it "iterates through a list for case 2" $ do
      rollingChunks' 3 "abcdef" `shouldBe` [("abc", 'd'), ("bcd", 'e'), ("cde", 'f')]
