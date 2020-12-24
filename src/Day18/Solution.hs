module Day18.Solution where

import Advent.Parser
import Advent.Utils
import Text.Parsec

part1 :: String -> String
part1 = show . sum . map (evaluateExpression . fromRightOrShowError . parseExpression) . lines

part2 :: String -> String
part2 = head . lines

data Operator = Plus | Times deriving (Show, Eq)

data Expression
  = BinaryOp (Expression, Operator, Expression)
  | Number Int
  deriving (Show, Eq)

parseExpression :: String -> Either ParseError Expression
parseExpression = parse expressionParser ""

expressionParser :: Parsec String () Expression
expressionParser = (parensParser <|> numberExpressionParser) `chainl1` operatorParser
  where
    parensParser :: Parsec String () Expression
    parensParser = between (char '(') (char ')') expressionParser

    numberExpressionParser :: Parsec String () Expression
    numberExpressionParser = Number <$> intParser

    operatorParser :: Parsec String () (Expression -> Expression -> Expression)
    operatorParser = readOperator <$> (space *> oneOf ['+', '*'] <* space)

    readOperator :: Char -> Expression -> Expression -> Expression
    readOperator '+' a b = BinaryOp (a, Plus, b)
    readOperator '*' a b = BinaryOp (a, Times, b)

evaluateExpression :: Expression -> Int
evaluateExpression (Number n) = n
evaluateExpression (BinaryOp (a, Plus, b)) = evaluateExpression a + evaluateExpression b
evaluateExpression (BinaryOp (a, Times, b)) = evaluateExpression a * evaluateExpression b
