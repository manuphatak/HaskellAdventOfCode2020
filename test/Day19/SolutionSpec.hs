{-# LANGUAGE TemplateHaskellQuotes #-}

module Day19.SolutionSpec (spec) where

import Data.Either
import Data.Foldable (for_)
import qualified Data.IntMap.Lazy as IntMap
import Day19.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day19/input.txt"
    part1 input `shouldBe` "216"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day19/input.txt"
    part2 input `shouldBe` "hello_santa"

  let exampleRules1 =
        IntMap.fromList
          [ (0, Ref [[1, 2]]),
            (1, Val 'a'),
            (2, Ref [[1, 3], [3, 1]]),
            (3, Val 'b')
          ]
  let exampleRules2 =
        IntMap.fromList
          [ (0, Ref [[4, 1, 5]]),
            (1, Ref [[2, 3], [3, 2]]),
            (2, Ref [[4, 4], [5, 5]]),
            (3, Ref [[4, 5], [5, 4]]),
            (4, Val 'a'),
            (5, Val 'b')
          ]
  let exampleDocument1 = Document exampleRules1 []
  let exampleDocument2 = Document exampleRules2 ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"]
  describe "parseDocument" $ do
    context "given example-1.txt" $ do
      it "parses rules" $ do
        input <- readFile "./test/Day19/example-1.txt"
        parseDocument input `shouldBe` Right exampleDocument1
    context "given example-2.txt" $ do
      it "parses rules" $ do
        input <- readFile "./test/Day19/example-2.txt"

        parseDocument input `shouldBe` Right exampleDocument2

  describe "dynamicParser" $ do
    context "given example-2.txt" $ do
      let parseDynamic = buildDynamicParser exampleRules2
      let cases =
            [ ("ababbb", isRight),
              ("bababa", isLeft),
              ("abbbab", isRight),
              ("aaabbb", isLeft),
              ("aaaabbb", isLeft)
            ]
      let test (input, matcher) = it ("satisfies matcher for input " ++ show input) $ do
            parseDynamic input `shouldSatisfy` matcher
      for_ cases test

  describe "validMessages" $ do
    context "given example-2.txt" $ do
      it "selects valid messages" $ do
        validMessages exampleDocument2 `shouldBe` ["ababbb", "abbbab"]
