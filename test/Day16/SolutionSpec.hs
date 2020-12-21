module Day16.SolutionSpec (spec) where

import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Day16.Solution
import Test.Hspec

spec :: Spec
spec =
  parallel $ do
    it "solves Part 1" $ do
      input <- readFile "./test/Day16/input.txt"
      part1 input `shouldBe` "21081"
    xit "solves Part 2" $ do
      input <- readFile "./test/Day16/input.txt"
      part2 input `shouldBe` "hello santa"
    let exampleDocument =
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
    describe "parseDocument" $ do
      it "parses rules, your ticket, and nearby tickets" $ do
        input <- readFile "./test/Day16/example.txt"
        parseDocument input `shouldBe` Right exampleDocument

    describe "ticketScanningErrors" $ do
      it "finds invalid ticket values" $ do
        let output = ticketScanningErrors exampleDocument
        output `shouldBe` [4, 55, 12]
        sum output `shouldBe` 71
