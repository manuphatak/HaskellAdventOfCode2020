module Day19.Solution where

import Advent.Parser
import Advent.Utils
import Control.Applicative
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Debug.Trace
import Text.Parsec hiding ((<|>))

part1 :: String -> String
part1 = show . length . validMessages . fromRightOrShowError . parseDocument

part2 :: String -> String
part2 = show . length . validMessages . withNewRules . fromRightOrShowError . parseDocument

data Rule = Branch [Int] [Int] | Ref [Int] | Val Char deriving (Show, Eq)

-- type Rules = IntMap Rule

data Document = Document {dRules :: IntMap Rule, dMessages :: [String]} deriving (Show, Eq)

parseDocument :: String -> Either ParseError Document
parseDocument = parse documentParser ""
  where
    documentParser :: Parsec String () Document
    documentParser = Document <$> rulesParser <*> (spaces *> messagesParser <* eof)

    messagesParser :: Parsec String () [String]
    messagesParser = many1 letter `sepEndBy` newline

    rulesParser :: Parsec String () (IntMap Rule)
    rulesParser = IntMap.fromList <$> (rulePairParser `endBy1` newline)

    rulePairParser :: Parsec String () (Int, Rule)
    rulePairParser = (,) <$> (intParser <* char ':' <* char ' ') <*> ruleParser

    ruleParser :: Parsec String () Rule
    ruleParser = choice [try branchParser, valParser, refParser]

    branchParser :: Parsec String () Rule
    branchParser = Branch <$> subRuleParser <*> (char '|' *> char ' ' *> subRuleParser)

    refParser :: Parsec String () Rule
    refParser = Ref <$> subRuleParser

    subRuleParser :: Parsec String () [Int]
    subRuleParser = intParser `sepEndBy1` char ' '

    valParser :: Parsec String () Rule
    valParser = Val <$> betweenDblQuotes letter

    betweenDblQuotes :: Parsec String () a -> Parsec String () a
    betweenDblQuotes = between (char '"') (char '"')

isValidMessage :: IntMap Rule -> String -> Bool
isValidMessage rules = (== Just "") . go 0
  where
    go :: Int -> String -> Maybe String
    go i = match (rules IntMap.! i)

    match :: Rule -> String -> Maybe String
    match (Val _) "" = Nothing
    match (Val c) (x : xs)
      | c == x = Just xs
      | otherwise = Nothing
    match (Ref (x : xs)) input = go x input >>= match (Ref xs)
    match (Ref []) input = Just input
    match (Branch ls rs) input = match (Ref ls) input <|> match (Ref rs) input

withNewRules :: Document -> Document
withNewRules document@Document {dRules = rules} = document {dRules = newRules}
  where
    newRules :: IntMap Rule
    newRules =
      foldr
        (uncurry IntMap.insert)
        rules
        [ (8, Branch [42] [42, 8]),
          (11, Branch [42, 31] [42, 11, 31])
        ]

validMessages :: Document -> [String]
validMessages (Document rules messages) = filter (isValidMessage rules) messages