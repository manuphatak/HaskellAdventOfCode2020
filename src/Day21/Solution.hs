{-# LANGUAGE TupleSections #-}

module Day21.Solution where

import Advent.Utils
import Control.Monad
import Data.Function
import Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . ap allergenFreeCount (asFoodAllergenMap . asKnowledgeGroup) . fromRightOrShowError . parseFoods

part2 :: String -> String
part2 = head . lines

type Ingredient = String

type Ingredients = Set.Set Ingredient

type Allergen = String

type Allergens = Set.Set Allergen

data Food = Food {fIngredients :: Ingredients, fAllergens :: Allergens} deriving (Show, Eq)

parseFoods :: String -> Either ParseError [Food]
parseFoods = parse (foodParser `sepEndBy1` newline) ""
  where
    foodParser :: Parsec String () Food
    foodParser = Food <$> ingredientsParser <*> allergensParser

    ingredientsParser :: Parsec String () Ingredients
    ingredientsParser = Set.fromList <$> many1 letter `sepEndBy1` space

    allergensParser :: Parsec String () Allergens
    allergensParser = Set.fromList <$> between (string "(contains ") (char ')') (many1 letter `sepBy` string ", ")

type FoodAllergenKnowledge = Map.Map Allergen Ingredients

asKnowledgeGroup :: [Food] -> FoodAllergenKnowledge
asKnowledgeGroup =
  Map.fromListWith Set.intersection . concatMap possibilities
  where
    possibilities :: Food -> [(Allergen, Ingredients)]
    possibilities food = food & fAllergens & Set.toList <&> (,fIngredients food)

type FoodAllergenMap = Map.Map Ingredient Allergen

asFoodAllergenMap :: FoodAllergenKnowledge -> FoodAllergenMap
asFoodAllergenMap knowledge = head . filter (\m -> Map.size m == Map.size knowledge) . map (invertMap . Map.fromList) . foldr (go . expandPair) [] . Map.toList $ knowledge
  where
    go :: [(Allergen, Ingredient)] -> [[(Ingredient, Allergen)]] -> [[(Ingredient, Allergen)]]
    go ps [] = map (: []) ps
    go xs ys = [x : y | x <- xs, y <- ys]

    expandPair :: (Allergen, Ingredients) -> [(Allergen, Ingredient)]
    expandPair (allergen, ingredientSet) = map (allergen,) . Set.toList $ ingredientSet

invertMap :: Ord k => Map.Map a k -> Map.Map k a
invertMap = Map.foldrWithKey invert Map.empty
  where
    invert :: (Ord k) => a -> k -> Map.Map k a -> Map.Map k a
    invert value key = Map.insert key value

allergenFreeCount :: [Food] -> FoodAllergenMap -> Int
allergenFreeCount foods allergenMap = map countIngredients ingredientSets & sum
  where
    allergenFree :: Ingredients
    allergenFree = ingredientSets & Set.unions & Set.filter (`Map.notMember` allergenMap)

    ingredientSets :: [Ingredients]
    ingredientSets = foods & map fIngredients

    countIngredients :: Ingredients -> Int

    countIngredients foodIngredients = Set.intersection foodIngredients allergenFree & Set.size
