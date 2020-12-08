module Day07.SolutionSpec (spec) where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Day07.Solution (Rules, Tree (..), asTree, expand, parseRules, part1, part2, pathsToTarget)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day07/input.txt"
    part1 input `shouldBe` "hello santa"
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

  describe "asTree" $ do
    context "given the parsedExample" $ do
      it "builds a Tree" $ do
        asTree "shiny gold" parsedExample
          `shouldBe` Map.fromList
            [ ( "bright white",
                [ Leaf (1, "shiny gold")
                ]
              ),
              ( "dark olive",
                [ Tree (3, "faded blue") (Just []),
                  Tree (4, "dotted black") (Just [])
                ]
              ),
              ( "dark orange",
                [ Tree (3, "bright white") (Just [Leaf (1, "shiny gold")]),
                  Tree
                    (4, "muted yellow")
                    ( Just
                        [ Leaf (2, "shiny gold"),
                          Tree (9, "faded blue") (Just [])
                        ]
                    )
                ]
              ),
              ("dotted black", []),
              ("faded blue", []),
              ( "light red",
                [ Tree (1, "bright white") (Just [Leaf (1, "shiny gold")]),
                  Tree
                    (2, "muted yellow")
                    ( Just
                        [ Leaf (2, "shiny gold"),
                          Tree (9, "faded blue") (Just [])
                        ]
                    )
                ]
              ),
              ( "muted yellow",
                [ Leaf (2, "shiny gold"),
                  Tree (9, "faded blue") (Just [])
                ]
              ),
              ( "shiny gold",
                [ Tree
                    (1, "dark olive")
                    ( Just
                        [ Tree (3, "faded blue") (Just []),
                          Tree (4, "dotted black") (Just [])
                        ]
                    ),
                  Tree
                    (2, "vibrant plum")
                    ( Just
                        [ Tree (5, "faded blue") (Just []),
                          Tree (6, "dotted black") (Just [])
                        ]
                    )
                ]
              ),
              ( "vibrant plum",
                [ Tree (5, "faded blue") (Just []),
                  Tree (6, "dotted black") (Just [])
                ]
              )
            ]

  describe "expand" $ do
    let cases =
          [ ( Leaf (1, "shiny gold"),
              [ [(1, "shiny gold")]
              ]
            ),
            ( Tree
                (4, "muted yellow")
                ( Just
                    [ Leaf (2, "shiny gold"),
                      Tree (9, "faded blue") (Just [])
                    ]
                ),
              [ [(4, "muted yellow"), (2, "shiny gold")],
                [(4, "muted yellow"), (9, "faded blue")]
              ]
            )
          ]
        test (input, expected) = it "expands a Tree into a path" $ do
          expand input `shouldBe` expected
     in for_ cases test
  xdescribe "pathsToTarget" $ do
    context "given the parsedExample" $ do
      it "finds paths to the target" $ do
        pathsToTarget "shiny gold" parsedExample
          `shouldBe` Map.fromList
            [ ("light red", Just [[(1, "bright white"), (1, "shiny gold")], [(2, "muted yellow"), (2, "shiny gold")]]),
              ("dark orange", Just [[(3, "bright white"), (1, "shiny gold")], [(4, "muted yellow"), (2, "shiny gold")]]),
              ("bright white", Just [[(1, "shiny gold")]]),
              ("muted yellow", Just [[(2, "shiny gold")]]),
              ("shiny gold", Just []),
              ("dark olive", Nothing),
              ("vibrant plum", Nothing),
              ("faded blue", Nothing),
              ("dotted black", Nothing)
            ]
