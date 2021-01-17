module Day19.SolutionSpec (spec) where

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
          [ (0, Ref (Or [And [Leaf 1, Leaf 2]])),
            (1, Val 'a'),
            (2, Ref (Or [And [Leaf 1, Leaf 3], And [Leaf 3, Leaf 1]])),
            (3, Val 'b')
          ]
  let exampleRules2 =
        IntMap.fromList
          [ (0, Ref (Or [And [Leaf 4, Leaf 1, Leaf 5]])),
            (1, Ref (Or [And [Leaf 2, Leaf 3], And [Leaf 3, Leaf 2]])),
            (2, Ref (Or [And [Leaf 4, Leaf 4], And [Leaf 5, Leaf 5]])),
            (3, Ref (Or [And [Leaf 4, Leaf 5], And [Leaf 5, Leaf 4]])),
            (4, Val 'a'),
            (5, Val 'b')
          ]
  let exampleRules3 =
        IntMap.fromList
          [ (0, Ref (Or [And [Leaf 8, Leaf 11]])),
            (1, Val 'a'),
            (2, Ref (Or [And [Leaf 1, Leaf 24], And [Leaf 14, Leaf 4]])),
            (3, Ref (Or [And [Leaf 5, Leaf 14], And [Leaf 16, Leaf 1]])),
            (4, Ref (Or [And [Leaf 1, Leaf 1]])),
            (5, Ref (Or [And [Leaf 1, Leaf 14], And [Leaf 15, Leaf 1]])),
            (6, Ref (Or [And [Leaf 14, Leaf 14], And [Leaf 1, Leaf 14]])),
            (7, Ref (Or [And [Leaf 14, Leaf 5], And [Leaf 1, Leaf 21]])),
            (8, Ref (Or [And [Leaf 42]])),
            (9, Ref (Or [And [Leaf 14, Leaf 27], And [Leaf 1, Leaf 26]])),
            (10, Ref (Or [And [Leaf 23, Leaf 14], And [Leaf 28, Leaf 1]])),
            (11, Ref (Or [And [Leaf 42, Leaf 31]])),
            (12, Ref (Or [And [Leaf 24, Leaf 14], And [Leaf 19, Leaf 1]])),
            (13, Ref (Or [And [Leaf 14, Leaf 3], And [Leaf 1, Leaf 12]])),
            (14, Val 'b'),
            (15, Ref (Or [And [Leaf 1], And [Leaf 14]])),
            (16, Ref (Or [And [Leaf 15, Leaf 1], And [Leaf 14, Leaf 14]])),
            (17, Ref (Or [And [Leaf 14, Leaf 2], And [Leaf 1, Leaf 7]])),
            (18, Ref (Or [And [Leaf 15, Leaf 15]])),
            (19, Ref (Or [And [Leaf 14, Leaf 1], And [Leaf 14, Leaf 14]])),
            (20, Ref (Or [And [Leaf 14, Leaf 14], And [Leaf 1, Leaf 15]])),
            (21, Ref (Or [And [Leaf 14, Leaf 1], And [Leaf 1, Leaf 14]])),
            (22, Ref (Or [And [Leaf 14, Leaf 14]])),
            (23, Ref (Or [And [Leaf 25, Leaf 1], And [Leaf 22, Leaf 14]])),
            (24, Ref (Or [And [Leaf 14, Leaf 1]])),
            (25, Ref (Or [And [Leaf 1, Leaf 1], And [Leaf 1, Leaf 14]])),
            (26, Ref (Or [And [Leaf 14, Leaf 22], And [Leaf 1, Leaf 20]])),
            (27, Ref (Or [And [Leaf 1, Leaf 6], And [Leaf 14, Leaf 18]])),
            (28, Ref (Or [And [Leaf 16, Leaf 1]])),
            (31, Ref (Or [And [Leaf 14, Leaf 17], And [Leaf 1, Leaf 13]])),
            (42, Ref (Or [And [Leaf 9, Leaf 14], And [Leaf 10, Leaf 1]]))
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

  describe "isValidMessage" $ do
    context "given example-2.txt" $ do
      let cases =
            [ ("ababbb", True),
              ("bababa", False),
              ("abbbab", True),
              ("aaabbb", False),
              ("aaaabbb", False)
            ]
      let test (input, expected) = it ("is " ++ show expected ++ " for input " ++ show input) $ do
            isValidMessage exampleRules2 input `shouldBe` expected
      for_ cases test
    context "given a modified example-3.txt" $ do
      let cases =
            [ (it, "bbabbbbaabaabba", True),
              (it, "babbbbaabbbbbabbbbbbaabaaabaaa", True),
              (it, "aaabbbbbbaaaabaababaabababbabaaabbababababaaa", True),
              (it, "bbbbbbbaaaabbbbaaabbabaaa", True),
              (it, "bbbababbbbaaaaaaaabbababaaababaabab", True),
              (it, "ababaaaaaabaaab", True),
              (it, "ababaaaaabbbaba", True),
              (it, "baabbaaaabbaaaababbaababb", True),
              (it, "abbbbabbbbaaaababbbbbbaaaababb", True),
              (it, "aaaaabbaabaaaaababaa", True),
              (it, "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", True),
              (it, "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba", True),
              (it, "aaaabbaaaabbaaa", False)
            ]
      let test (testIt, input, expected) = testIt ("is " ++ show expected ++ " for input " ++ show input) $ do
            example3 <- readFile "./test/Day19/example-3.txt"
            let Right rules = dRules . withNewRules <$> parseDocument example3
            isValidMessage rules input `shouldBe` expected
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
