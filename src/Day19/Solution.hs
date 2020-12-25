module Day19.Solution where

import Advent.Parser
import Advent.Utils
import Data.Either
import qualified Data.IntMap.Lazy as IntMap
import Text.Parsec

part1 :: String -> String
part1 = show . length . validMessages . fromRightOrShowError . parseDocument

part2 :: String -> String
part2 = head . lines

data Rule = Ref [[Int]] | Val Char deriving (Show, Eq)

type Rules = IntMap.IntMap Rule

data Document = Document {dRules :: Rules, dMessages :: [String]} deriving (Show, Eq)

parseDocument :: String -> Either ParseError Document
parseDocument = parse documentParser ""
  where
    documentParser :: Parsec String () Document
    documentParser = Document <$> rulesParser <*> (spaces *> messagesParser <* eof)

    messagesParser :: Parsec String () [String]
    messagesParser = many1 letter `sepEndBy` newline

    rulesParser :: Parsec String () Rules
    rulesParser = IntMap.fromList <$> (rulePairParser `endBy1` newline)

    rulePairParser :: Parsec String () (Int, Rule)
    rulePairParser = (,) <$> (intParser <* char ':' <* char ' ') <*> ruleParser

    ruleParser :: Parsec String () Rule
    ruleParser = valParser <|> refParser

    refParser :: Parsec String () Rule
    refParser = Ref <$> (subRuleParser `sepBy1` (char '|' <* char ' '))

    subRuleParser :: Parsec String () [Int]
    subRuleParser = intParser `sepEndBy1` char ' '

    valParser :: Parsec String () Rule
    valParser = Val <$> betweenDblQuotes letter

    betweenDblQuotes :: Parsec String () a -> Parsec String () a
    betweenDblQuotes = between (char '"') (char '"')

buildDynamicParser :: Rules -> String -> Either ParseError String
buildDynamicParser rules = parse dynamicParser ""
  where
    dynamicParser :: Parsec String () String
    dynamicParser = go (rules IntMap.! 0) <* eof
      where
        go :: Rule -> Parsec String () String
        go (Ref options) = choice . map (try . mconcat . map (go . (rules IntMap.!))) $ options
        go (Val c) = string [c]

-- parseTemp :: String -> Either ParseError String
-- parseTemp = parse tempParser ""
--   where
--     tempParser :: Parsec String () String
--     tempParser =
--       string "a"
--         <> choice
--           [ try
--               ( choice
--                   [ try (string "a" <> string "a"),
--                     try (string "b" <> string "b")
--                   ]
--                   <> choice
--                     [ try (string "a" <> string "b"),
--                       try (string "b" <> string "a")
--                     ]
--               ),
--             try
--               ( choice
--                   [ try (string "a" <> string "b"),
--                     try (string "b" <> string "a")
--                   ]
--                   <> choice
--                     [ try (string "a" <> string "a"),
--                       try (string "b" <> string "b")
--                     ]
--               )
--           ]
--         <> string "b"
--         <* eof

validMessages :: Document -> [String]
validMessages (Document rules messages) = filter (isRight . parseDynamic) messages
  where
    parseDynamic = buildDynamicParser rules
