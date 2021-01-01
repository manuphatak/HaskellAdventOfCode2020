module Day21.SolutionSpec (spec) where

import qualified Data.Set as Set
import Day21.Solution
import Test.Hspec

spec :: Spec
spec = focus . parallel $ do
  xit "solves Part 1" $ do
    input <- readFile "./test/Day21/input.txt"
    part1 input `shouldBe` "hello_santa"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day21/input.txt"
    part2 input `shouldBe` "hello_santa"

  describe "parseFoods" $ do
    it "can parse the example" $ do
      input <- readFile "./test/Day21/example.txt"
      let exampleFoods =
            [ Food (Set.fromList ["kfcds", "mxmxvkd", "nhms", "sqjhc"]) (Set.fromList ["dairy", "fish"]),
              Food (Set.fromList ["fvjkl", "mxmxvkd", "sbzzf", "trh"]) (Set.fromList ["dairy"]),
              Food (Set.fromList ["fvjkl", "sqjhc"]) (Set.fromList ["soy"]),
              Food (Set.fromList ["mxmxvkd", "sbzzf", "sqjhc"]) (Set.fromList ["fish"])
            ]
      parseFoods input `shouldBe` Right exampleFoods
