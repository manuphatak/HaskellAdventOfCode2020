module Day21.SolutionSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Day21.Solution
  ( Food (..),
    allergenFreeCount,
    asCanonicalDangerousIngredientList,
    asFoodAllergenMap,
    asKnowledgeGroup,
    parseFoods,
    part1,
    part2,
  )
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day21/input.txt"
    part1 input `shouldBe` "2556"

  it "solves Part 2" $ do
    input <- readFile "./test/Day21/input.txt"
    part2 input `shouldBe` "vcckp,hjz,nhvprqb,jhtfzk,mgkhhc,qbgbmc,bzcrknb,zmh"

  let exampleFoods =
        [ Food (Set.fromList ["kfcds", "mxmxvkd", "nhms", "sqjhc"]) (Set.fromList ["dairy", "fish"]),
          Food (Set.fromList ["fvjkl", "mxmxvkd", "sbzzf", "trh"]) (Set.fromList ["dairy"]),
          Food (Set.fromList ["fvjkl", "sqjhc"]) (Set.fromList ["soy"]),
          Food (Set.fromList ["mxmxvkd", "sbzzf", "sqjhc"]) (Set.fromList ["fish"])
        ]
  describe "parseFoods" $ do
    it "can parse the example" $ do
      input <- readFile "./test/Day21/example.txt"
      parseFoods input `shouldBe` Right exampleFoods

  let exampleKnowledgeGroup =
        Map.fromList
          [ ("dairy", Set.fromList ["mxmxvkd"]),
            ("fish", Set.fromList ["mxmxvkd", "sqjhc"]),
            ("soy", Set.fromList ["sqjhc", "fvjkl"])
          ]
  describe "asKnowledgeGroup" $ do
    it "is the combined intersection of foods" $ do
      asKnowledgeGroup exampleFoods `shouldBe` exampleKnowledgeGroup

  let exampleAllergenMap =
        Map.fromList
          [ ("mxmxvkd", "dairy"),
            ("sqjhc", "fish"),
            ("fvjkl", "soy")
          ]
  describe "asFoodAllergenMap" $ do
    it "creates a list of possible food allergy mappings" $ do
      asFoodAllergenMap exampleKnowledgeGroup `shouldBe` Just exampleAllergenMap

  describe "allergenFreeCount" $ do
    it "finds the foods not included in the allergen map" $ do
      allergenFreeCount exampleFoods exampleAllergenMap `shouldBe` 5

  describe "asCanonicalDangerousIngredientList" $ do
    it "is a sorted, comma separated list of ingredients" $ do
      asCanonicalDangerousIngredientList exampleAllergenMap `shouldBe` "mxmxvkd,sqjhc,fvjkl"
