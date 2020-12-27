module Day20.SolutionSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Sequence
import Day20.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day20/input.txt"
    part1 input `shouldBe` "21599955909991"
  it "solves Part 2" $ do
    input <- readFile "./test/Day20/input.txt"
    part2 input `shouldBe` "2495"

  let exampleTiles =
        fromList
          [ Tile {tId = 2311, tContent = ["..##.#..#.", "##..#.....", "#...##..#.", "####.#...#", "##.##.###.", "##...#.###", ".#.#.#..##", "..#....#..", "###...#.#.", "..###..###"]},
            Tile {tId = 1951, tContent = ["#.##...##.", "#.####...#", ".....#..##", "#...######", ".##.#....#", ".###.#####", "###.##.##.", ".###....#.", "..#.#..#.#", "#...##.#.."]},
            Tile {tId = 1171, tContent = ["####...##.", "#..##.#..#", "##.#..#.#.", ".###.####.", "..###.####", ".##....##.", ".#...####.", "#.##.####.", "####..#...", ".....##..."]},
            Tile {tId = 1427, tContent = ["###.##.#..", ".#..#.##..", ".#.##.#..#", "#.#.#.##.#", "....#...##", "...##..##.", "...#.#####", ".#.####.#.", "..#..###.#", "..##.#..#."]},
            Tile {tId = 1489, tContent = ["##.#.#....", "..##...#..", ".##..##...", "..#...#...", "#####...#.", "#..#.#.#.#", "...#.#.#..", "##.#...##.", "..##.##.##", "###.##.#.."]},
            Tile {tId = 2473, tContent = ["#....####.", "#..#.##...", "#.##..#...", "######.#.#", ".#...#.#.#", ".#########", ".###.#..#.", "########.#", "##...##.#.", "..###.#.#."]},
            Tile {tId = 2971, tContent = ["..#.#....#", "#...###...", "#.#.###...", "##.##..#..", ".#####..##", ".#..####.#", "#..#.#..#.", "..####.###", "..#.#.###.", "...#.#.#.#"]},
            Tile {tId = 2729, tContent = ["...#.#.#.#", "####.#....", "..#.#.....", "....#..#.#", ".##..##.#.", ".#.####...", "####.#.#..", "##.####...", "##..#.##..", "#.##...##."]},
            Tile {tId = 3079, tContent = ["#.#.#####.", ".#..######", "..#.......", "######....", "####.#..#.", ".#...#.##.", "#.#####.##", "..#.###...", "..#.......", "..#.###..."]}
          ]

  describe "parseTiles" $ do
    it "parses the input into tiles" $ do
      input <- readFile "./test/Day20/example.txt"

      parseTiles input `shouldBe` Right exampleTiles

  let exampleGrid =
        Map.fromList
          [ ((-1, -2), Tile {tId = 2971, tContent = ["..#.#....#", "#...###...", "#.#.###...", "##.##..#..", ".#####..##", ".#..####.#", "#..#.#..#.", "..####.###", "..#.#.###.", "...#.#.#.#"]}),
            ((-1, -1), Tile {tId = 2729, tContent = ["...#.#.#.#", "####.#....", "..#.#.....", "....#..#.#", ".##..##.#.", ".#.####...", "####.#.#..", "##.####...", "##..#.##..", "#.##...##."]}),
            ((-1, 0), Tile {tId = 1951, tContent = ["#.##...##.", "#.####...#", ".....#..##", "#...######", ".##.#....#", ".###.#####", "###.##.##.", ".###....#.", "..#.#..#.#", "#...##.#.."]}),
            ((0, -2), Tile {tId = 1489, tContent = ["##.#.#....", "..##...#..", ".##..##...", "..#...#...", "#####...#.", "#..#.#.#.#", "...#.#.#..", "##.#...##.", "..##.##.##", "###.##.#.."]}),
            ((0, -1), Tile {tId = 1427, tContent = ["###.##.#..", ".#..#.##..", ".#.##.#..#", "#.#.#.##.#", "....#...##", "...##..##.", "...#.#####", ".#.####.#.", "..#..###.#", "..##.#..#."]}),
            ((0, 0), Tile {tId = 2311, tContent = ["..##.#..#.", "##..#.....", "#...##..#.", "####.#...#", "##.##.###.", "##...#.###", ".#.#.#..##", "..#....#..", "###...#.#.", "..###..###"]}),
            ((1, -2), Tile {tId = 1171, tContent = ["...##.....", "...#..####", ".####.##.#", ".####...#.", ".##....##.", "####.###..", ".####.###.", ".#.#..#.##", "#..#.##..#", ".##...####"]}),
            ((1, -1), Tile {tId = 2473, tContent = [".##...####", ".######...", "#.###.##..", "#.###.###.", "#.#.#.#...", ".######.##", "###.#..###", "..#.###..#", "##.##....#", "..#.###..."]}),
            ((1, 0), Tile {tId = 3079, tContent = ["..#.###...", "..#.......", "..#.###...", "#.#####.##", ".#...#.##.", "####.#..#.", "######....", "..#.......", ".#..######", "#.#.#####."]})
          ]
  describe "buildGrid" $ do
    it "places all the pieces" $ do
      buildGrid exampleTiles `shouldBe` exampleGrid

  describe "boundingBox" $ do
    context "given a grid" $ do
      it "is the 4 corners of the box" $ do
        boundingBox exampleGrid `shouldBe` [(-1, -2), (-1, 0), (1, -2), (1, 0)]

  describe "cornerIds" $ do
    context "given a grid" $ do
      it "is the ids of 4 corners" $ do
        cornerIds exampleGrid `shouldBe` [2971, 1951, 1171, 3079]
  describe "tileFit" $ do
    it "finds a position and orientation for a tile" $ do
      let grid = Map.fromList [((0, 0), Tile {tId = 2311, tContent = ["..##.#..#.", "##..#.....", "#...##..#.", "####.#...#", "##.##.###.", "##...#.###", ".#.#.#..##", "..#....#..", "###...#.#.", "..###..###"]})] :: Grid
      let tile = Tile {tId = 1427, tContent = ["###.##.#..", ".#..#.##..", ".#.##.#..#", "#.#.#.##.#", "....#...##", "...##..##.", "...#.#####", ".#.####.#.", "..#..###.#", "..##.#..#."]}
      let point = (0, -1) :: Point
      tileFit grid point tile `shouldBe` Just tile

  describe "orientations" $ do
    it "is all the orientations of a 2d grid" $ do
      let square =
            [ "abcd",
              "efgh",
              "ijkl",
              "mnoo"
            ]

      orientations square
        `shouldBe` [ [ "abcd",
                       "efgh",
                       "ijkl",
                       "mnoo"
                     ],
                     [ "miea",
                       "njfb",
                       "okgc",
                       "olhd"
                     ],
                     [ "oonm",
                       "lkji",
                       "hgfe",
                       "dcba"
                     ],
                     [ "dhlo",
                       "cgko",
                       "bfjn",
                       "aeim"
                     ],
                     [ "dcba",
                       "hgfe",
                       "lkji",
                       "oonm"
                     ],
                     [ "olhd",
                       "okgc",
                       "njfb",
                       "miea"
                     ],
                     [ "mnoo",
                       "ijkl",
                       "efgh",
                       "abcd"
                     ],
                     [ "aeim",
                       "bfjn",
                       "cgko",
                       "dhlo"
                     ]
                   ]

  describe "stitchedImage" $ do
    context "given a grid of tiles" $ do
      it "is a single image" $ do
        stitchedImage exampleGrid
          `shouldBe` [ "...###...##...#...#..###",
                       ".#.###..##..##..####.##.",
                       "#.##..#..#...#..####...#",
                       "#####..#####...###....##",
                       "#..####...#.#.#.###.###.",
                       "..#.#..#..#.#.#.####.###",
                       ".####.###.#...###.#..#.#",
                       ".#.#.###.##.##.#..#.##..",
                       "###.#...#..#.##.######..",
                       ".#.#....#.##.#...###.##.",
                       "...#..#..#.#.##..###.###",
                       "##..##.#...#...#.#.#.#..",
                       "#.####....##..########.#",
                       "###.#.#...#.######.#..##",
                       "#.####..#.####.#.#.###..",
                       "#..#.##..#..###.#.##....",
                       ".####...#..#.....#......",
                       "....#..#...##..#.#.###..",
                       "...########.#....#####.#",
                       "##.#....#.##.####...#.##",
                       "###.#####...#.#####.#..#",
                       "##.##.###.#.#..######...",
                       "###....#.#....#..#......",
                       ".#.#..#.##...#.##..#####"
                     ]
  describe "stripEdges" $ do
    it "removes the borders of an image" $ do
      let img = ["xxxx", "xiix", "xiix", "xxxx"]

      stripEdges img
        `shouldBe` [ "ii",
                     "ii"
                   ]

  describe "combineImages" $ do
    context "given an array of images" $ do
      it "combines into one image" $ do
        let images :: [[Image]]
            images =
              [ [ [ "aaa",
                    "aaa",
                    "aaa"
                  ],
                  [ "bbb",
                    "bbb",
                    "bbb"
                  ],
                  [ "ccc",
                    "ccc",
                    "ccc"
                  ]
                ],
                [ [ "ddd",
                    "ddd",
                    "ddd"
                  ],
                  [ "eee",
                    "eee",
                    "eee"
                  ],
                  [ "fff",
                    "fff",
                    "fff"
                  ]
                ],
                [ [ "ggg",
                    "ggg",
                    "ggg"
                  ],
                  [ "hhh",
                    "hhh",
                    "hhh"
                  ],
                  [ "iii",
                    "iii",
                    "iii"
                  ]
                ]
              ]

        combineImages images
          `shouldBe` [ "aaabbbccc",
                       "aaabbbccc",
                       "aaabbbccc",
                       "dddeeefff",
                       "dddeeefff",
                       "dddeeefff",
                       "ggghhhiii",
                       "ggghhhiii",
                       "ggghhhiii"
                     ]

  describe "countSeaMonsters" $ do
    context "given an image" $ do
      it "can match sea monsters" $ do
        let exampleImage =
              [ ".####...#####..#...###..",
                "#####..#..#.#.####..#.#.",
                ".#.#...#.###...#.##.##..",
                "#.#.##.###.#.##.##.#####",
                "..##.###.####..#.####.##",
                "...#.#..##.##...#..#..##",
                "#.##.#..#.#..#..##.#.#..",
                ".###.##.....#...###.#...",
                "#.####.#.#....##.#..#.#.",
                "##...#..#....#..#...####",
                "..#.##...###..#.#####..#",
                "....#.##.#.#####....#...",
                "..##.##.###.....#.##..#.",
                "#...#...###..####....##.",
                ".#.##...#.##.#.#.###...#",
                "#.###.#..####...##..#...",
                "#.###...#.##...#.######.",
                ".###.###.#######..#####.",
                "..##.#..#..#.#######.###",
                "#.#..##.########..#..##.",
                "#.#####..#.#...##..#....",
                "#....##..#.#########..##",
                "#...#.....#..##...###.##",
                "#..###....##.#...##.##.#"
              ]

        countSeaMonsters exampleImage `shouldBe` 2
    context "given an example grid" $ do
      it "can match sea monsters" $ do
        (maxSeaMonsterCount . stitchedImage) exampleGrid `shouldBe` 2

-- describe "findSeaMonster" $ do
--   context "when the lines have a sea monster" $ do
--     it "finds the sea monster" $ do
--       let lines =
--             [ "##.##.###.#.#..######...",
--               "###.#####...#.#####.#..#",
--               "##.#....#.##.####...#.##"
--             ]
--       findSeaMonster lines `shouldBe` True
--   context "when the lines do not have a sea monster" $ do
--     it "does not find the sea monster" $ do
--       let lines =
--             [ "###....#.#....#..#......",
--               "##.##.###.#.#..######...",
--               "###.#####...#.#####.#..#"
--             ]
--       findSeaMonster lines `shouldBe` False
