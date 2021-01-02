{-# LANGUAGE TupleSections #-}

module Day21.Solution
  ( Food (..),
    allergenFreeCount,
    asCanonicalDangerousIngredientList,
    asFoodAllergenMap,
    asKnowledgeGroup,
    parseFoods,
    part1,
    part2,
  )
where

import Advent.Utils (fromRightOrShowError)
import Control.Monad (ap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = show . ap allergenFreeCount (fromJust . asFoodAllergenMap . asKnowledgeGroup) . fromRightOrShowError . parseFoods

part2 :: String -> String
part2 = asCanonicalDangerousIngredientList . fromJust . asFoodAllergenMap . asKnowledgeGroup . fromRightOrShowError . parseFoods

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

asFoodAllergenMap :: FoodAllergenKnowledge -> Maybe FoodAllergenMap
asFoodAllergenMap knowledge =
  knowledge
    & Map.toList
    & foldr (permutations . expandPair) []
    & map (invertMap . Map.fromList)
    & filter (`eqLength` knowledge)
    & safeHead
  where
    permutations :: [(Allergen, Ingredient)] -> [[(Ingredient, Allergen)]] -> [[(Ingredient, Allergen)]]
    permutations ps [] = map (: []) ps
    permutations xs ys = [x : y | x <- xs, y <- ys]

    expandPair :: (Allergen, Ingredients) -> [(Allergen, Ingredient)]
    expandPair (allergen, ingredientSet) = ingredientSet & Set.toList & map (allergen,)

    eqLength l r = length l == length r

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
    countIngredients foodIngredients = Set.intersection foodIngredients allergenFree & length

asCanonicalDangerousIngredientList :: FoodAllergenMap -> String
asCanonicalDangerousIngredientList = intercalate "," . map fst . sortOn snd . Map.toList

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
