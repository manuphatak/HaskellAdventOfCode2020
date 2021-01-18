module Day22.SolutionSpec (spec) where

import qualified Data.Sequence as Seq
import Day22.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day22/input.txt"
    part1 input `shouldBe` "30197"

  it "solves Part 2" $ do
    input <- readFile "./test/Day22/input.txt"
    part2 input `shouldBe` "34031"

  let example1Decks = (Seq.fromList [9, 2, 6, 3, 1], Seq.fromList [5, 8, 4, 7, 10])
  let example2Decks = (Seq.fromList [43, 19], Seq.fromList [2, 29, 14])
  describe "parseDecks" $ do
    context "given example-1.txt" $ do
      it "parses the example" $ do
        input <- readFile "./test/Day22/example-1.txt"
        parseDecks input `shouldBe` Right example1Decks
    context "given example-2.txt" $ do
      it "parses the example" $ do
        input <- readFile "./test/Day22/example-2.txt"
        parseDecks input `shouldBe` Right example2Decks

  let example1SimpleCombatResults = (Player2, Seq.fromList [3, 2, 10, 6, 8, 5, 9, 4, 7, 1])
  let example2SimpleCombatResults = (Player1, Seq.fromList [43, 19])
  let example1RecursiveCombatResults = (Player2, Seq.fromList [7, 5, 6, 2, 4, 1, 10, 8, 9, 3])
  let example2RecursiveCombatResults = (Player1, Seq.fromList [43, 19])

  describe "playGameWith" $ do
    context "given the simpleCombat handler" $ do
      context "given example-1.txt" $ do
        it "runs the game until one side loses" $ do
          uncurry (playGameWith simpleCombat) example1Decks `shouldBe` example1SimpleCombatResults
      context "given example-2.txt" $ do
        it "runs the game until one side loses" $ do
          uncurry (playGameWith simpleCombat) example2Decks `shouldBe` example2SimpleCombatResults
  context "given the recursiveCombat handler" $ do
    context "given example-1.txt" $ do
      it "runs the game until one side loses" $ do
        uncurry (playGameWith recursiveCombat) example1Decks `shouldBe` example1RecursiveCombatResults
    context "given example-2.txt" $ do
      it "runs the game until one side loses" $ do
        uncurry (playGameWith recursiveCombat) example2Decks `shouldBe` example2RecursiveCombatResults

  describe "winningScore" $ do
    context "given example-1.txt" $ do
      it "calculates the winning score" $ do
        winningScore example1SimpleCombatResults `shouldBe` 306

      it "calculates the winning score" $ do
        winningScore example1RecursiveCombatResults `shouldBe` 291
