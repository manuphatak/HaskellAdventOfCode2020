module Day12.Solution where

import Advent.Utils (readInt)
import Text.Parsec

part1 :: String -> String
part1 = head . lines

part2 :: String -> String
part2 = head . lines

type Value = Int

data Action = N | E | S | W | F | R | L deriving (Show, Eq, Read)

type Instruction = (Action, Value)

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instructionParser `sepEndBy1` endOfLine) ""

instructionParser :: Parsec String () Instruction
instructionParser = (,) <$> actionParser <*> valueParser

actionParser :: Parsec String () Action
actionParser = read . (: []) <$> oneOf ['N', 'E', 'S', 'W', 'F', 'R', 'L']

valueParser :: Parsec String () Value
valueParser = readInt <$> many digit
