module Day07.Solution
  ( Rules,
    Tree (..),
    asPath,
    asTree,
    countBags,
    flattenPaths,
    parseRules,
    part1,
    part2,
    pathsToTarget,
  )
where

import Advent.Utils (fromRightOrError', readInt)
import qualified Data.Map.Strict as Map
import Day08.Utils (fromRightOrError')
import Debug.Trace
import Text.Parsec

part1 :: String -> String
part1 = show . pathsToTarget "shiny gold" . fromRightOrError' . parseRules

part2 :: String -> String
part2 = show . countBags "shiny gold" . asTree . fromRightOrError' . parseRules

type Bag = String

type Rules = Map.Map Bag [(Int, Bag)]

newtype Tree a = Tree [(a, Tree a)] deriving (Show, Eq)

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

pathsToTarget :: Bag -> Rules -> Int
pathsToTarget target = Map.size . Map.filter containsTarget . flattenPaths
  where
    containsTarget :: [[(Int, Bag)]] -> Bool
    containsTarget = any (any (\(_, bag) -> bag == target))

flattenPaths :: Rules -> Map.Map Bag [[(Int, Bag)]]
flattenPaths = Map.map asPath . asTree

asTree :: Rules -> Map.Map Bag (Tree (Int, Bag))
asTree rules = Map.mapWithKey (\key _ -> fn key (Tree [])) rules
  where
    fn :: Bag -> Tree (Int, Bag) -> Tree (Int, Bag)
    fn key history = Tree $ map (\kid@(_, nextKey) -> (kid, fn nextKey history)) kids
      where
        kids = rules Map.! key

asPath :: Tree a -> [[a]]
asPath (Tree nodes) = concatMap walkNode nodes
  where
    walkNode :: (a, Tree a) -> [[a]]
    walkNode (a, tree) = go [a] tree
    go :: [a] -> Tree a -> [[a]]
    go history (Tree []) = [history]
    go history (Tree nodes') = concatMap (\(a, tree) -> go (a : history) tree) nodes'

countBags :: Bag -> Map.Map Bag (Tree (Int, Bag)) -> Int
countBags target = go . (Map.! target)
  where
    go :: Tree (Int, Bag) -> Int
    go (Tree []) = 1
    go (Tree nodes) = sum $ traceShowId $ map (uncurry go') nodes
    go' :: (Int, b) -> Tree (Int, Bag) -> Int
    go' node (Tree []) = fst node
    go' node tree = traceShow (fst node, go tree) $ (fst node) * (go tree) + (fst node)
