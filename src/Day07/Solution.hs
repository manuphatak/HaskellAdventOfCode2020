module Day07.Solution (part1, part2, parseRules, Rules, pathsToTarget, Tree (..), asTree, expand) where

import Advent.Utils (readInt)
import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Text.Parsec

part1 :: String -> String
part1 = head . lines

part2 :: String -> String
part2 = head . lines

type Bag = String

type Rules = Map.Map Bag [(Int, Bag)]

parseRules :: String -> Either ParseError Rules
parseRules = parse (Map.fromList <$> try ruleParser `sepEndBy1` newline) ""

ruleParser :: Parsec String () (Bag, [(Int, Bag)])
ruleParser =
  ((,) <$> (bagParser <* string " bags contain") <*> choice [try containsNoBagsParser, containsBagsParser]) <* char '.'

containsNoBagsParser :: Parsec String () [(Int, Bag)]
containsNoBagsParser = do
  _ <- string " no other bags"
  pure []

containsBagsParser :: Parsec String () [(Int, Bag)]
containsBagsParser = bagCountParser `sepBy1` char ','

bagCountParser :: Parsec String () (Int, Bag)
bagCountParser = (,) <$> countParser <*> bagParser'
  where
    countParser = space *> (readInt <$> many digit) <* space
    bagParser' = bagParser <* space <* skipMany1 letter

bagParser :: Parsec String () Bag
bagParser =
  manyTill anyChar $
    try $
      lookAhead $
        string " bag"

data Tree = Leaf (Int, Bag) | Tree (Int, Bag) (Maybe [Tree]) deriving (Show, Eq)

pathsToTarget :: Bag -> Rules -> Map.Map Bag (Maybe [[(Int, Bag)]])
pathsToTarget target = Map.map go
  where
    go :: [(Int, Bag)] -> Maybe [[(Int, Bag)]]
    go x = Just [x]

asTree :: Bag -> Rules -> Map.Map Bag [Tree]
asTree target rules = Map.map go rules
  where
    go :: [(Int, Bag)] -> [Tree]
    go [] = []
    go (x@(_, bag) : xs)
      | bag == target = (Leaf x) : go xs
      | otherwise = Tree x (go <$> (Map.lookup bag rules)) : (go xs)

expand :: Tree -> [[(Int, Bag)]]
expand _t = go ([], _t)
  where
    go :: ([[(Int, Bag)]], Tree) -> [[(Int, Bag)]]
    go ([], Leaf x) = [[x]]
    go (acc, Leaf x) = map ((:) x) acc
    go (acc, Tree x (Just trees)) =
      let expanded = (join $ map (\_ -> go (acc, Leaf x)) trees)
       in (join $ map (\tree -> go (expanded, tree)) trees)

-- expand (Tree x (Maybe trees)) =

-- pathsToTarget target rules = map (pathToTarget target rules) $ Map.keys rules

-- -- pathToTarget :: Bag -> Rules -> Bag -> [Maybe [(Int, Bag)]]
-- pathToTarget target rules key = go [] key
--   where
--     -- go :: [(Int, Bag)] -> Bag -> Maybe [(Int, Bag)]
--     go visited key
--       | key == target = Just visited
--       | otherwise = (\values -> (go (sequenceA (head values) : visited) key)) <$> Map.lookup target rules

-- temp = (map (\(v@(_, nextKey)) -> go (v : visited) nextKey))

-- [Maybe a] -> Maybe [a]

-- >>> input <- readFile "./test/Day07/example.txt"
-- >>> pathsToTarget "shiny gold" <$> (parseRules $ input)
-- Right (fromList [("bright white",Just [[(1,"shiny gold")]]),("dark olive",Nothing),("dark orange",Nothing),("dotted black",Nothing),("faded blue",Nothing),("light red",Nothing),("muted yellow",Just [[(2,"shiny gold")]]),("shiny gold",Nothing),("vibrant plum",Nothing)])
