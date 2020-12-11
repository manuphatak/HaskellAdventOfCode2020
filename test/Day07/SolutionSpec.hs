module Day07.SolutionSpec (spec) where

import qualified Data.Map.Strict as Map
import Day07.Solution
  ( Rules,
    Tree (..),
    asPath,
    asTree,
    expandPath,
    expandPaths,
    parseRules,
    part1,
    part2,
    pathsToTarget,
  )
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day07/input.txt"
    part1 input `shouldBe` "192"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day07/input.txt"
    part2 input `shouldBe` "hello santa"

  let parsedExample =
        Map.fromList
          [ ("light red", [(1, "bright white"), (2, "muted yellow")]),
            ("dark orange", [(3, "bright white"), (4, "muted yellow")]),
            ("bright white", [(1, "shiny gold")]),
            ("muted yellow", [(2, "shiny gold"), (9, "faded blue")]),
            ("shiny gold", [(1, "dark olive"), (2, "vibrant plum")]),
            ("dark olive", [(3, "faded blue"), (4, "dotted black")]),
            ("vibrant plum", [(5, "faded blue"), (6, "dotted black")]),
            ("faded blue", []),
            ("dotted black", [])
          ] ::
          Rules

  describe "parseRules" $
    context "given the file example.txt" $
      it "parses the rules" $ do
        input <- readFile "./test/Day07/example.txt"

        parseRules input `shouldBe` Right parsedExample
  describe "expandPath" $ do
    it "appends a value to each of it's children" $ do
      expandPath 'a' ["bc", "cd"] `shouldBe` ["abc", "acd"]
  describe "expandPaths" $ do
    it "appends a value to each of it's children" $ do
      expandPaths "ab" ["cd", "ef"] `shouldBe` ["acd", "aef", "bcd", "bef"]

  describe "pathsToTarget" $ do
    context "given the parsedExample" $ do
      it "finds paths to the target" $ do
        pathsToTarget "shiny gold" parsedExample
          `shouldBe` Map.fromList
            [ ("bright white", [[(1, "shiny gold")]]),
              ("dark orange", [[(1, "shiny gold"), (3, "bright white")], [(2, "shiny gold"), (4, "muted yellow")]]),
              ("light red", [[(1, "shiny gold"), (1, "bright white")], [(2, "shiny gold"), (2, "muted yellow")]]),
              ("muted yellow", [[(2, "shiny gold")]])
            ]
  describe "asTree" $ do
    context "given the parsedExample" $ do
      it "creates a tree" $ do
        asTree "shiny gold" parsedExample
          `shouldBe` Map.fromList
            [ ("bright white", Tree [((1, "shiny gold"), Leaf)]),
              ("dark olive", Tree [((3, "faded blue"), Tree []), ((4, "dotted black"), Tree [])]),
              ("dark orange", Tree [((3, "bright white"), Tree [((1, "shiny gold"), Leaf)]), ((4, "muted yellow"), Tree [((2, "shiny gold"), Leaf), ((9, "faded blue"), Tree [])])]),
              ("dotted black", Tree []),
              ("faded blue", Tree []),
              ("light red", Tree [((1, "bright white"), Tree [((1, "shiny gold"), Leaf)]), ((2, "muted yellow"), Tree [((2, "shiny gold"), Leaf), ((9, "faded blue"), Tree [])])]),
              ("muted yellow", Tree [((2, "shiny gold"), Leaf), ((9, "faded blue"), Tree [])]),
              ("shiny gold", Leaf),
              ( "vibrant plum",
                Tree
                  [ ((5, "faded blue"), Tree []),
                    ((6, "dotted black"), Tree [])
                  ]
              )
            ]

  describe "asPath" $ do
    it "flattens a tree into a list of paths for case 1" $ do
      let tree = Tree [('a', Tree [('c', Leaf)])]
      asPath tree `shouldBe` ["ca"]

    it "flattens a tree into a list of paths for case 2" $ do
      let tree = Tree [('b', Tree [('d', Leaf), ('e', Leaf)])]
      asPath tree `shouldBe` ["db", "eb"]

    it "flattens a tree into a list of paths for case 3" $ do
      let tree = Tree [('a', Tree [('c', Leaf)]), ('b', Tree [('d', Tree [('f', Leaf)]), ('e', Leaf)])]
      asPath tree `shouldBe` ["ca", "fdb", "eb"]
