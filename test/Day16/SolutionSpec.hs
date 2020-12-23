module Day16.SolutionSpec (spec) where

import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Day16.Solution
import Test.Hspec

spec :: Spec
spec = focus $
  parallel $ do
    it "solves Part 1" $ do
      input <- readFile "./test/Day16/input.txt"
      part1 input `shouldBe` "21081"
    fit "solves Part 2" $ do
      input <- readFile "./test/Day16/input.txt"
      part2 input `shouldBe` "314360510573"
    let exampleDocument1 =
          Document
            { dFields =
                Map.fromList
                  [ ("class", IntSet.fromList ([1 .. 3] ++ [5 .. 7])),
                    ("row", IntSet.fromList ([6 .. 11] ++ [33 .. 44])),
                    ("seat", IntSet.fromList ([13 .. 40] ++ [45 .. 50]))
                  ],
              dYourTicket = toIntMap [7, 1, 14],
              dNearbyTickets =
                [ toIntMap [7, 3, 47],
                  toIntMap [40, 4, 50],
                  toIntMap [55, 2, 20],
                  toIntMap [38, 6, 12]
                ]
            }
    let validDocument1 = ValidDocument exampleDocument1 {dNearbyTickets = [toIntMap [7, 3, 47]]}
    let exampleDocument2 =
          Document
            { dFields =
                Map.fromList
                  [ ("class", IntSet.fromList ([0 .. 1] ++ [4 .. 19])),
                    ("row", IntSet.fromList ([0 .. 5] ++ [8 .. 19])),
                    ("seat", IntSet.fromList ([0 .. 13] ++ [16 .. 19]))
                  ],
              dYourTicket = toIntMap [11, 12, 13],
              dNearbyTickets =
                [ toIntMap [3, 9, 18],
                  toIntMap [15, 1, 5],
                  toIntMap [5, 14, 9]
                ]
            }
    let validDocument2 = ValidDocument exampleDocument2
    describe "parseDocument" $ do
      context "given example-1.txt" $ do
        it "parses rules, your ticket, and nearby tickets" $ do
          input <- readFile "./test/Day16/example-1.txt"
          parseDocument input `shouldBe` Right exampleDocument1
      context "given example-2.txt" $ do
        it "parses rules, your ticket, and nearby tickets" $ do
          input <- readFile "./test/Day16/example-2.txt"
          parseDocument input `shouldBe` Right exampleDocument2

    describe "ticketScanningErrors" $ do
      context "given example-1.txt" $ do
        it "finds invalid ticket values" $ do
          let output = ticketScanningErrors exampleDocument1
          output `shouldBe` [4, 55, 12]
          sum output `shouldBe` 71
      context "given example-2.txt" $ do
        it "finds invalid ticket values" $ do
          let output = ticketScanningErrors exampleDocument2
          output `shouldBe` []
          sum output `shouldBe` 0

    describe "asValidDocument" $ do
      context "given example-1.txt" $ do
        it "selects only valid tickets" $ do
          asValidDocument exampleDocument1 `shouldBe` validDocument1
      context "given example-2.txt" $ do
        it "selects only valid tickets" $ do
          asValidDocument exampleDocument2 `shouldBe` validDocument2

    describe "extractTicket" $ do
      context "given example-1.txt" $ do
        it "selects only valid tickets" $ do
          extractTicket validDocument2 `shouldBe` Map.fromList [("class", 12), ("row", 11), ("seat", 13)]

    describe "resolveColumns" $ do
      it "solves columns mappings" $ do
        let input = Map.fromList [("class", [1, 2]), ("row", [0, 1, 2]), ("seat", [2])]
        let expected = Map.fromList [("class", 1), ("row", 0), ("seat", 2)]

        resolveColumns input `shouldBe` expected
