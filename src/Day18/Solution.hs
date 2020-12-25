module Day18.Solution where

import Advent.Parser
import Advent.Utils
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr

part1 :: String -> String
part1 = show . sum . map (evaluateExpression . fromRightOrShowError . parseExpression basicTable) . lines

part2 :: String -> String
part2 = show . sum . map (evaluateExpression . fromRightOrShowError . parseExpression advancedTable) . lines

data Op = Plus | Times deriving (Show, Eq)

data Expression
  = BinaryOp (Expression, Op, Expression)
  | Number Int
  deriving (Show, Eq)

type ExpressionOpTable = OperatorTable String () Identity Expression

evaluateExpression :: Expression -> Int
evaluateExpression (Number n) = n
evaluateExpression (BinaryOp (a, Plus, b)) = evaluateExpression a + evaluateExpression b
evaluateExpression (BinaryOp (a, Times, b)) = evaluateExpression a * evaluateExpression b

parseExpression :: ExpressionOpTable -> String -> Either ParseError Expression
parseExpression table = parse (expressionParser table) ""

expressionParser :: ExpressionOpTable -> Parsec String () Expression
expressionParser table = buildExpressionParser table termParser
  where
    termParser = parensParser <|> numberExpressionParser

    parensParser :: Parsec String () Expression
    parensParser = between (char '(') (char ')') (expressionParser table)

    numberExpressionParser :: Parsec String () Expression
    numberExpressionParser = Number <$> intParser

basicTable :: ExpressionOpTable
basicTable = [[Infix operatorParser AssocLeft]]
  where
    operatorParser :: Parsec String () (Expression -> Expression -> Expression)
    operatorParser = readOperator <$> (space *> oneOf ['+', '*'] <* space)

advancedTable :: ExpressionOpTable
advancedTable =
  [ [Infix (try plusParser) AssocLeft],
    [Infix (try timesParser) AssocLeft]
  ]
  where
    timesParser :: Parsec String () (Expression -> Expression -> Expression)
    timesParser = readOperator <$> (space *> char '*' <* space)
    plusParser :: Parsec String () (Expression -> Expression -> Expression)
    plusParser = readOperator <$> (space *> char '+' <* space)

readOperator :: Char -> Expression -> Expression -> Expression
readOperator '+' a b = BinaryOp (a, Plus, b)
readOperator '*' a b = BinaryOp (a, Times, b)

-- drawExpression :: Expression -> String
-- drawExpression (BinaryOp (a, b, c)) = unlines ["  ( " ++ show a, "    " ++ show b, "    " ++ show c, "  )"]
-- drawExpression expression = show expression

-- temp =
--   BinaryOp
--     ( Number 2,
--       Times,
--       BinaryOp
--         ( Number 3,
--           Plus,
--           BinaryOp
--             ( Number 4,
--               Times,
--               Number 5
--             )
--         )
--     )
