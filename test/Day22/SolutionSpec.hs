module Day22.SolutionSpec (spec) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Day22.Solution
import Test.Hspec

spec :: Spec
spec = focus . parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day22/input.txt"
    part1 input `shouldBe` "30197"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day22/input.txt"
    -- failed 33957 (too low)
    part2 input `shouldBe` "hello_santa"

  let exampleGame1 = (Seq.fromList [9, 2, 6, 3, 1], Seq.fromList [5, 8, 4, 7, 10])
  let exampleGame2 = (Seq.fromList [43, 19], Seq.fromList [2, 29, 14])
  describe "parseGame" $ do
    context "given example-1.txt" $ do
      it "parses the example" $ do
        input <- readFile "./test/Day22/example-1.txt"
        parseGame input `shouldBe` Right exampleGame1
    context "given example-2.txt" $ do
      it "parses the example" $ do
        input <- readFile "./test/Day22/example-2.txt"
        parseGame input `shouldBe` Right exampleGame2
  let exampleGameFinal1 = (Seq.empty, Seq.fromList [3, 2, 10, 6, 8, 5, 9, 4, 7, 1]) :: Game
  let exampleGameFinalRecursive1 = (Seq.empty, Seq.fromList [7, 5, 6, 2, 4, 1, 10, 8, 9, 3]) :: Game
  let exampleGameFinal2 = (Seq.fromList [43, 19], Seq.empty) :: Game
  let exampleGameFinalRecursive2 = (Seq.fromList [43, 19], Seq.empty) :: Game
  describe "combat" $ do
    context "given example-1.txt" $ do
      it "runs the game until one side loses" $ do
        combat exampleGame1 `shouldBe` exampleGameFinal1
    xcontext "given example-2.txt" $ do
      it "runs the game until one side loses" $ do
        combat exampleGame2 `shouldBe` exampleGameFinal2
  fdescribe "recursiveCombat" $ do
    context "given example-1.txt" $ do
      it "runs the game until one side loses" $ do
        fst (recursiveCombat 1 1 Set.empty exampleGame1) `shouldBe` exampleGameFinalRecursive1
    xcontext "given example-2.txt" $ do
      it "runs the game until one side loses" $ do
        fst (recursiveCombat 1 1 Set.empty exampleGame2) `shouldBe` exampleGameFinalRecursive2

  describe "winningScore" $ do
    context "given example-1.txt" $ do
      it "calculates the winning score" $ do
        winningScore exampleGameFinal1 `shouldBe` 306
      it "calculates the winning score" $ do
        winningScore exampleGameFinalRecursive1 `shouldBe` 291
