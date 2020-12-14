module Day07.SolutionSpec (spec) where

import Advent.Utils (fromRightOrShowError)
import qualified Data.Map.Strict as Map
import Day07.Solution
  ( Rules,
    Tree (..),
    asPath,
    asTree,
    countBags,
    flattenPaths,
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
  it "solves Part 2" $ do
    input <- readFile "./test/Day07/input.txt"
    part2 input `shouldBe` "12128"

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
    context "given the file example-1.txt" $
      it "parses the rules" $ do
        input <- readFile "./test/Day07/example-1.txt"

        parseRules input `shouldBe` Right parsedExample

  describe "pathsToTarget" $ do
    context "given the parsedExample" $ do
      it "finds paths to a target" $ do
        pathsToTarget "shiny gold" parsedExample `shouldBe` 4

  describe "countBags" $ do
    context "given the file example-1.txt" $ do
      it "counts all of the bags required to fill the target" $ do
        input <- asTree . fromRightOrShowError . parseRules <$> readFile "./test/Day07/example-1.txt"

        countBags "shiny gold" input `shouldBe` 32
    context "given the file example-2.txt" $ do
      it "counts all of the bags required to fill the target" $ do
        input <- asTree . fromRightOrShowError . parseRules <$> readFile "./test/Day07/example-2.txt"

        countBags "shiny gold" input `shouldBe` 126

  describe "flattenPaths" $ do
    context "given the parsedExample" $ do
      it "finds paths to the target" $ do
        flattenPaths parsedExample
          `shouldBe` Map.fromList
            [ ( "bright white",
                [ [(3, "faded blue"), (1, "dark olive"), (1, "shiny gold")],
                  [(4, "dotted black"), (1, "dark olive"), (1, "shiny gold")],
                  [(5, "faded blue"), (2, "vibrant plum"), (1, "shiny gold")],
                  [(6, "dotted black"), (2, "vibrant plum"), (1, "shiny gold")]
                ]
              ),
              ( "dark olive",
                [ [(3, "faded blue")],
                  [(4, "dotted black")]
                ]
              ),
              ( "dark orange",
                [ [(3, "faded blue"), (1, "dark olive"), (1, "shiny gold"), (3, "bright white")],
                  [(4, "dotted black"), (1, "dark olive"), (1, "shiny gold"), (3, "bright white")],
                  [(5, "faded blue"), (2, "vibrant plum"), (1, "shiny gold"), (3, "bright white")],
                  [(6, "dotted black"), (2, "vibrant plum"), (1, "shiny gold"), (3, "bright white")],
                  [(3, "faded blue"), (1, "dark olive"), (2, "shiny gold"), (4, "muted yellow")],
                  [(4, "dotted black"), (1, "dark olive"), (2, "shiny gold"), (4, "muted yellow")],
                  [(5, "faded blue"), (2, "vibrant plum"), (2, "shiny gold"), (4, "muted yellow")],
                  [(6, "dotted black"), (2, "vibrant plum"), (2, "shiny gold"), (4, "muted yellow")],
                  [(9, "faded blue"), (4, "muted yellow")]
                ]
              ),
              ("dotted black", []),
              ("faded blue", []),
              ( "light red",
                [ [(3, "faded blue"), (1, "dark olive"), (1, "shiny gold"), (1, "bright white")],
                  [(4, "dotted black"), (1, "dark olive"), (1, "shiny gold"), (1, "bright white")],
                  [(5, "faded blue"), (2, "vibrant plum"), (1, "shiny gold"), (1, "bright white")],
                  [(6, "dotted black"), (2, "vibrant plum"), (1, "shiny gold"), (1, "bright white")],
                  [(3, "faded blue"), (1, "dark olive"), (2, "shiny gold"), (2, "muted yellow")],
                  [(4, "dotted black"), (1, "dark olive"), (2, "shiny gold"), (2, "muted yellow")],
                  [(5, "faded blue"), (2, "vibrant plum"), (2, "shiny gold"), (2, "muted yellow")],
                  [(6, "dotted black"), (2, "vibrant plum"), (2, "shiny gold"), (2, "muted yellow")],
                  [(9, "faded blue"), (2, "muted yellow")]
                ]
              ),
              ( "muted yellow",
                [ [(3, "faded blue"), (1, "dark olive"), (2, "shiny gold")],
                  [(4, "dotted black"), (1, "dark olive"), (2, "shiny gold")],
                  [(5, "faded blue"), (2, "vibrant plum"), (2, "shiny gold")],
                  [(6, "dotted black"), (2, "vibrant plum"), (2, "shiny gold")],
                  [(9, "faded blue")]
                ]
              ),
              ( "shiny gold",
                [ [(3, "faded blue"), (1, "dark olive")],
                  [(4, "dotted black"), (1, "dark olive")],
                  [(5, "faded blue"), (2, "vibrant plum")],
                  [(6, "dotted black"), (2, "vibrant plum")]
                ]
              ),
              ( "vibrant plum",
                [ [(5, "faded blue")],
                  [(6, "dotted black")]
                ]
              )
            ]
  describe "asTree" $ do
    context "given the parsedExample" $ do
      it "creates a tree" $ do
        asTree parsedExample
          `shouldBe` Map.fromList
            [ ( "bright white",
                Tree
                  [ ( (1, "shiny gold"),
                      Tree
                        [ ( (1, "dark olive"),
                            Tree
                              [ ((3, "faded blue"), Tree []),
                                ((4, "dotted black"), Tree [])
                              ]
                          ),
                          ( (2, "vibrant plum"),
                            Tree
                              [ ((5, "faded blue"), Tree []),
                                ((6, "dotted black"), Tree [])
                              ]
                          )
                        ]
                    )
                  ]
              ),
              ( "dark olive",
                Tree
                  [ ((3, "faded blue"), Tree []),
                    ((4, "dotted black"), Tree [])
                  ]
              ),
              ( "dark orange",
                Tree
                  [ ( (3, "bright white"),
                      Tree
                        [ ( (1, "shiny gold"),
                            Tree
                              [ ( (1, "dark olive"),
                                  Tree
                                    [ ((3, "faded blue"), Tree []),
                                      ((4, "dotted black"), Tree [])
                                    ]
                                ),
                                ( (2, "vibrant plum"),
                                  Tree
                                    [ ((5, "faded blue"), Tree []),
                                      ((6, "dotted black"), Tree [])
                                    ]
                                )
                              ]
                          )
                        ]
                    ),
                    ( (4, "muted yellow"),
                      Tree
                        [ ( (2, "shiny gold"),
                            Tree
                              [ ( (1, "dark olive"),
                                  Tree
                                    [ ((3, "faded blue"), Tree []),
                                      ((4, "dotted black"), Tree [])
                                    ]
                                ),
                                ( (2, "vibrant plum"),
                                  Tree
                                    [ ((5, "faded blue"), Tree []),
                                      ((6, "dotted black"), Tree [])
                                    ]
                                )
                              ]
                          ),
                          ((9, "faded blue"), Tree [])
                        ]
                    )
                  ]
              ),
              ("dotted black", Tree []),
              ("faded blue", Tree []),
              ( "light red",
                Tree
                  [ ( (1, "bright white"),
                      Tree
                        [ ( (1, "shiny gold"),
                            Tree
                              [ ( (1, "dark olive"),
                                  Tree
                                    [ ((3, "faded blue"), Tree []),
                                      ((4, "dotted black"), Tree [])
                                    ]
                                ),
                                ( (2, "vibrant plum"),
                                  Tree
                                    [ ((5, "faded blue"), Tree []),
                                      ((6, "dotted black"), Tree [])
                                    ]
                                )
                              ]
                          )
                        ]
                    ),
                    ( (2, "muted yellow"),
                      Tree
                        [ ( (2, "shiny gold"),
                            Tree
                              [ ( (1, "dark olive"),
                                  Tree
                                    [ ((3, "faded blue"), Tree []),
                                      ((4, "dotted black"), Tree [])
                                    ]
                                ),
                                ( (2, "vibrant plum"),
                                  Tree
                                    [ ((5, "faded blue"), Tree []),
                                      ((6, "dotted black"), Tree [])
                                    ]
                                )
                              ]
                          ),
                          ((9, "faded blue"), Tree [])
                        ]
                    )
                  ]
              ),
              ( "muted yellow",
                Tree
                  [ ( (2, "shiny gold"),
                      Tree
                        [ ( (1, "dark olive"),
                            Tree
                              [ ((3, "faded blue"), Tree []),
                                ((4, "dotted black"), Tree [])
                              ]
                          ),
                          ( (2, "vibrant plum"),
                            Tree
                              [ ((5, "faded blue"), Tree []),
                                ((6, "dotted black"), Tree [])
                              ]
                          )
                        ]
                    ),
                    ((9, "faded blue"), Tree [])
                  ]
              ),
              ( "shiny gold",
                Tree
                  [ ( (1, "dark olive"),
                      Tree
                        [ ((3, "faded blue"), Tree []),
                          ((4, "dotted black"), Tree [])
                        ]
                    ),
                    ( (2, "vibrant plum"),
                      Tree
                        [ ((5, "faded blue"), Tree []),
                          ((6, "dotted black"), Tree [])
                        ]
                    )
                  ]
              ),
              ( "vibrant plum",
                Tree
                  [ ((5, "faded blue"), Tree []),
                    ((6, "dotted black"), Tree [])
                  ]
              )
            ]

  describe "asPath" $ do
    it "flattens a tree into a list of paths for case 1" $ do
      let tree = Tree [('a', Tree [('c', Tree [])])]
      asPath tree `shouldBe` ["ca"]

    it "flattens a tree into a list of paths for case 2" $ do
      let tree = Tree [('b', Tree [('d', Tree []), ('e', Tree [])])]
      asPath tree `shouldBe` ["db", "eb"]

    it "flattens a tree into a list of paths for case 3" $ do
      let tree = Tree [('a', Tree [('c', Tree [])]), ('b', Tree [('d', Tree [('f', Tree [])]), ('e', Tree [])])]
      asPath tree `shouldBe` ["ca", "fdb", "eb"]
