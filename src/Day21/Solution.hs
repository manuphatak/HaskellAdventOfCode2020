module Day21.Solution where

import qualified Data.Set as Set
import Text.Parsec

part1 :: String -> String
part1 = head . lines

part2 :: String -> String
part2 = head . lines

type Ingredients = Set.Set String

type Allergens = Set.Set String

data Food = Food Ingredients Allergens deriving (Show, Eq)

parseFoods :: String -> Either ParseError [Food]
parseFoods = parse (foodParser `sepEndBy1` newline) ""
  where
    foodParser :: Parsec String () Food
    foodParser = Food <$> ingredientsParser <*> allergensParser

    ingredientsParser :: Parsec String () Ingredients
    ingredientsParser = Set.fromList <$> many1 letter `sepEndBy1` space

    allergensParser :: Parsec String () Allergens
    allergensParser = Set.fromList <$> between (string "(contains ") (char ')') (many1 letter `sepBy` string ", ")
