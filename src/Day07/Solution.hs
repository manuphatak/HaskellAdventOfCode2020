module Day07.Solution (part1, part2, parseRules, Rule (..)) where

import Advent.Utils (readInt)
import Text.Parsec

part1 :: String -> String
part1 = head . lines

part2 :: String -> String
part2 = head . lines

type Bag = String

data Rule = Rule Bag [(Int, Bag)] deriving (Show, Eq)

parseRules :: String -> Either ParseError [Rule]
parseRules = parse (try ruleParser `sepEndBy1` newline) ""

ruleParser :: Parsec String () Rule
ruleParser =
  (Rule <$> (bagParser <* string " bags contain") <*> choice [try containsNoBagsParser, containsBagsParser]) <* char '.'

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

-- >>> input <- readFile "./test/Day07/example.txt"
-- >>> parseRules input
-- Right [Rule "light red" [(1,"bright white"),(2,"muted yellow")],Rule "dark orange" [(3,"bright white"),(4,"muted yellow")],Rule "bright white" [(1,"shiny gold")],Rule "muted yellow" [(2,"shiny gold"),(9,"faded blue")],Rule "shiny gold" [(1,"dark olive"),(2,"vibrant plum")],Rule "dark olive" [(3,"faded blue"),(4,"dotted black")],Rule "vibrant plum" [(5,"faded blue"),(6,"dotted black")],Rule "faded blue" [],Rule "dotted black" []]
