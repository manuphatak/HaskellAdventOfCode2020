module Day07.Solution
  ( part1,
    part2,
    parseRules,
    Rules,
    expandPath,
    expandPaths,
    pathsToTarget,
    asTree,
    Tree (..),
    asPath,
  )
where

import Advent.Utils (readInt)
import qualified Data.Map.Strict as Map
import Day08.Utils (fromRightOrError')
import Text.Parsec

part1 :: String -> String
part1 = show . Map.size . pathsToTarget "shiny gold" . fromRightOrError' . parseRules

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

expandPath :: a -> [[a]] -> [[a]]
expandPath item = map (item :)

expandPaths :: [a] -> [[a]] -> [[a]]
expandPaths = foldr go (const [])
  where
    go :: a -> ([[a]] -> [[a]]) -> [[a]] -> [[a]]
    go x = (<*>) ((++) <$> map (x :))

pathsToTarget :: Bag -> Rules -> Map.Map Bag [[(Int, Bag)]]
pathsToTarget target = Map.filter (not . null) . Map.map asPath . asTree target

data Tree a = Tree [(a, Tree a)] | Leaf deriving (Show, Eq)

asTree :: Bag -> Rules -> Map.Map Bag (Tree (Int, Bag))
asTree target rules = Map.mapWithKey (\key _ -> fn key Leaf) rules
  where
    fn :: Bag -> Tree (Int, Bag) -> Tree (Int, Bag)
    -- fn _ (Tree []) = (Tree [])
    fn key history
      | key == target = history
      | otherwise = Tree $ map (\kid@(_, nextKey) -> (kid, fn nextKey history)) kids
      where
        kids = rules Map.! key

asPath :: Tree a -> [[a]]
asPath Leaf = []
asPath (Tree nodes) = concatMap walkNode nodes
  where
    walkNode :: (a, Tree a) -> [[a]]
    walkNode (a, tree) = go [a] tree
    go :: [a] -> Tree a -> [[a]]
    go history (Tree nodes') = concatMap (\(a, tree) -> go (a : history) tree) nodes'
    go history Leaf = [history]
