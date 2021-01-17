module Day19.SolutionSpec (spec) where

import Data.Either
import Data.Foldable (for_)
import qualified Data.IntMap.Lazy as IntMap
import Day19.Solution
import Test.Hspec

spec :: Spec
spec = focus . parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day19/input.txt"
    part1 input `shouldBe` "216"
  it "solves Part 2" $ do
    input <- readFile "./test/Day19/input.txt"
    -- Not 267
    -- Not 411
    part2 input `shouldBe` "hello_santa"

  let exampleRules1 =
        IntMap.fromList
          [ (0, Ref [1, 2]),
            (1, Val 'a'),
            (2, Branch [1, 3] [3, 1]),
            (3, Val 'b')
          ]
  let exampleRules2 =
        IntMap.fromList
          [ (0, Ref [4, 1, 5]),
            (1, Branch [2, 3] [3, 2]),
            (2, Branch [4, 4] [5, 5]),
            (3, Branch [4, 5] [5, 4]),
            (4, Val 'a'),
            (5, Val 'b')
          ]
  let exampleRules3 =
        IntMap.fromList
          [ (0, Ref [8, 11]),
            (1, Val 'a'),
            (2, Branch [1, 24] [14, 4]),
            (3, Branch [5, 14] [16, 1]),
            (4, Ref [1, 1]),
            (5, Branch [1, 14] [15, 1]),
            (6, Branch [14, 14] [1, 14]),
            (7, Branch [14, 5] [1, 21]),
            (8, Ref [42]),
            (9, Branch [14, 27] [1, 26]),
            (10, Branch [23, 14] [28, 1]),
            (11, Ref [42, 31]),
            (12, Branch [24, 14] [19, 1]),
            (13, Branch [14, 3] [1, 12]),
            (14, Val 'b'),
            (15, Branch [1] [14]),
            (16, Branch [15, 1] [14, 14]),
            (17, Branch [14, 2] [1, 7]),
            (18, Ref [15, 15]),
            (19, Branch [14, 1] [14, 14]),
            (20, Branch [14, 14] [1, 15]),
            (21, Branch [14, 1] [1, 14]),
            (22, Ref [14, 14]),
            (23, Branch [25, 1] [22, 14]),
            (24, Ref [14, 1]),
            (25, Branch [1, 1] [1, 14]),
            (26, Branch [14, 22] [1, 20]),
            (27, Branch [1, 6] [14, 18]),
            (28, Ref [16, 1]),
            (31, Branch [14, 17] [1, 13]),
            (42, Branch [9, 14] [10, 1])
          ]

  let exampleDocument1 = Document exampleRules1 []
  let exampleDocument2 = Document exampleRules2 ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"]
  let exampleDocument3 = Document exampleRules3 ["abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa", "bbabbbbaabaabba", "babbbbaabbbbbabbbbbbaabaaabaaa", "aaabbbbbbaaaabaababaabababbabaaabbababababaaa", "bbbbbbbaaaabbbbaaabbabaaa", "bbbababbbbaaaaaaaabbababaaababaabab", "ababaaaaaabaaab", "ababaaaaabbbaba", "baabbaaaabbaaaababbaababb", "abbbbabbbbaaaababbbbbbaaaababb", "aaaaabbaabaaaaababaa", "aaaabbaaaabbaaa", "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", "babaaabbbaaabaababbaabababaaab", "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]
  describe "parseDocument" $ do
    context "given example-1.txt" $ do
      it "parses rules" $ do
        input <- readFile "./test/Day19/example-1.txt"
        parseDocument input `shouldBe` Right exampleDocument1
    context "given example-2.txt" $ do
      it "parses rules" $ do
        input <- readFile "./test/Day19/example-2.txt"

        parseDocument input `shouldBe` Right exampleDocument2
    context "given example-3.txt" $ do
      it "parses rules" $ do
        input <- readFile "./test/Day19/example-3.txt"

        parseDocument input `shouldBe` Right exampleDocument3

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
    context "given a modified example-3.txt" $ do
      let cases =
            [ ("bbabbbbaabaabba", isRight),
              ("babbbbaabbbbbabbbbbbaabaaabaaa", isRight),
              ("aaabbbbbbaaaabaababaabababbabaaabbababababaaa", isRight),
              ("bbbbbbbaaaabbbbaaabbabaaa", isRight),
              ("bbbababbbbaaaaaaaabbababaaababaabab", isRight),
              ("ababaaaaaabaaab", isRight),
              ("ababaaaaabbbaba", isRight),
              ("baabbaaaabbaaaababbaababb", isRight),
              ("abbbbabbbbaaaababbbbbbaaaababb", isRight),
              ("aaaaabbaabaaaaababaa", isRight),
              ("aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", isRight),
              ("aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba", isRight),
              ("aaaabbaaaabbaaa", isLeft)
            ]
      let test (input, matcher) = it ("satisfies matcher for input " ++ show input) $ do
            example3 <- readFile "./test/Day19/example-3.txt"
            let Right parseDynamic = buildDynamicParser . dRules . withNewRules <$> parseDocument example3
            parseDynamic input `shouldSatisfy` matcher
      for_ cases test

  describe "validMessages" $ do
    context "given example-2.txt" $ do
      it "selects valid messages" $ do
        validMessages exampleDocument2 `shouldBe` ["ababbb", "abbbab"]
    context "given example-3.txt" $ do
      it "selects valid messages" $ do
        input <- readFile "./test/Day19/example-3.txt"
        let Right document = parseDocument input

        validMessages document `shouldBe` ["bbabbbbaabaabba", "ababaaaaaabaaab", "ababaaaaabbbaba"]
    context "given a modified example-3.txt" $ do
      it "selects valid messages" $ do
        input <- readFile "./test/Day19/example-3.txt"
        let Right document = withNewRules <$> parseDocument input

        validMessages document
          `shouldBe` [ "bbabbbbaabaabba",
                       "babbbbaabbbbbabbbbbbaabaaabaaa",
                       "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
                       "bbbbbbbaaaabbbbaaabbabaaa",
                       "bbbababbbbaaaaaaaabbababaaababaabab",
                       "ababaaaaaabaaab",
                       "ababaaaaabbbaba",
                       "baabbaaaabbaaaababbaababb",
                       "abbbbabbbbaaaababbbbbbaaaababb",
                       "aaaaabbaabaaaaababaa",
                       "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
                       "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
                     ]
