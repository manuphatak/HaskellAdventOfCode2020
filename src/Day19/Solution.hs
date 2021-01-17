module Day19.Solution where

import Advent.Parser
import Advent.Utils
import Control.Applicative
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Text.Parsec hiding ((<|>))

part1 :: String -> String
part1 = show . length . validMessages . fromRightOrShowError . parseDocument

part2 :: String -> String
part2 = show . length . validMessages . withNewRules . fromRightOrShowError . parseDocument

data Rule = Ref (AndOr Int) | Val Char deriving (Show, Eq)

data AndOr a
  = Leaf a
  | And [AndOr a]
  | Or [AndOr a]
  deriving (Show, Eq)

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
    ruleParser = choice [valParser, refParser]

    refParser :: Parsec String () Rule
    refParser = Ref <$> orParser

    orParser :: Parsec String () (AndOr Int)
    orParser = Or <$> andParser `sepBy1` (char '|' <* char ' ')

    andParser :: Parsec String () (AndOr Int)
    andParser = And <$> leafParser `sepEndBy1` char ' '

    leafParser :: Parsec String () (AndOr Int)
    leafParser = Leaf <$> intParser

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
    match (Ref andOr) input = matchAndOr andOr input

    matchAndOr :: AndOr Int -> String -> Maybe String
    matchAndOr (Or xs) input = foldr1 (<|>) . map (`matchAndOr` input) $ xs
    matchAndOr (And (x : xs)) input = matchAndOr x input >>= matchAndOr (And xs)
    matchAndOr (And []) input = Just input
    matchAndOr (Leaf x) input = go x input

withNewRules :: Document -> Document
withNewRules document@Document {dRules = rules} = document {dRules = newRules <> rules}
  where
    newRules :: IntMap Rule
    newRules =
      IntMap.fromList
        [ (8, Ref (Or [And [Leaf 42], And [Leaf 42, Leaf 8]])),
          (11, Ref (Or [And [Leaf 42, Leaf 31], And [Leaf 42, Leaf 11, Leaf 31]]))
        ]

validMessages :: Document -> [String]
validMessages (Document rules messages) = filter (isValidMessage rules) messages
