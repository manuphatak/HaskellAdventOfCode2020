module Day22.SolutionSpec (spec) where

import Data.Sequence
import Day22.Solution
import Test.Hspec

spec :: Spec
spec = focus . parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day22/input.txt"
    part1 input `shouldBe` "30197"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day22/input.txt"
    part2 input `shouldBe` "hello_santa"

  let exampleGame = Game (fromList [9, 2, 6, 3, 1]) (fromList [5, 8, 4, 7, 10]) 0
  describe "parseGame" $ do
    it "parses the example" $ do
      input <- readFile "./test/Day22/example.txt"
      parseGame input `shouldBe` Right exampleGame
  let exampleGameFinal = Game empty (fromList [3, 2, 10, 6, 8, 5, 9, 4, 7, 1]) 29
  describe "play" $ do
    it "runs the game until one side loses" $ do
      play exampleGame `shouldBe` exampleGameFinal

  describe "winningScore" $ do
    it "calculates the winning score" $ do
      winningScore exampleGameFinal `shouldBe` 306
