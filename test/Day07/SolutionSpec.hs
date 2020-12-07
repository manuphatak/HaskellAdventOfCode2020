module Day07.SolutionSpec (spec) where

import Day07.Solution (Rule (..), parseRules, part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day07/input.txt"
    part1 input `shouldBe` "hello santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day07/input.txt"
    part2 input `shouldBe` "hello santa"
  describe "parseRules" $
    context "given the file example.txt" $
      it "parses the rules" $ do
        input <- readFile "./test/Day07/example.txt"
        let expected =
              Right
                [ Rule "light red" [(1, "bright white"), (2, "muted yellow")],
                  Rule "dark orange" [(3, "bright white"), (4, "muted yellow")],
                  Rule "bright white" [(1, "shiny gold")],
                  Rule "muted yellow" [(2, "shiny gold"), (9, "faded blue")],
                  Rule "shiny gold" [(1, "dark olive"), (2, "vibrant plum")],
                  Rule "dark olive" [(3, "faded blue"), (4, "dotted black")],
                  Rule "vibrant plum" [(5, "faded blue"), (6, "dotted black")],
                  Rule "faded blue" [],
                  Rule "dotted black" []
                ]
        parseRules input `shouldBe` expected
