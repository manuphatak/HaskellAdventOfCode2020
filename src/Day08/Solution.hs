module Day08.Solution
  ( part1,
    part2,
    Instruction (..),
    Sign (..),
    Operation (..),
    Program (..),
    parseInstructions,
    runProgram,
    initialState,
  )
where

import Advent.Utils (readInt)
import Data.Either (fromRight)
import qualified Data.IntSet as IntSet
import Debug.Trace
import Text.Parsec

part1 :: String -> String
part1 = show . programAcc . runProgram initialState . fromRight [] . parseInstructions

part2 :: String -> String
part2 = head . lines

data Sign = Plus | Minus deriving (Show, Eq)

data Operation = NoOperation | Accumulator | Jump deriving (Show, Eq)

data Instruction = Instruction Operation Sign Int deriving (Show, Eq)

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instructionParser `sepEndBy1` endOfLine) ""

instructionParser :: Parsec String () Instruction
instructionParser = Instruction <$> (operationParser <* space) <*> signParser <*> intParser

operationParser :: Parsec String () Operation
operationParser = readOperation <$> many1 letter
  where
    readOperation "nop" = NoOperation
    readOperation "acc" = Accumulator
    readOperation "jmp" = Jump

signParser :: Parsec String () Sign
signParser = readSign <$> choice [char '+', char '-']
  where
    readSign '+' = Plus
    readSign '-' = Minus

intParser :: Parsec String () Int
intParser = readInt <$> many1 digit

data Program = Program {programAcc :: Int, programPointer :: Int, programVisited :: IntSet.IntSet} deriving (Show)

initialState :: Program
initialState = Program {programAcc = 0, programPointer = 0, programVisited = IntSet.empty}

runProgram :: Program -> [Instruction] -> Program
runProgram program instructions
  | programPointer program `IntSet.member` programVisited program = program
  | otherwise = runProgram (go (instructions !! programPointer program)) instructions
  where
    nextProgram = program {programVisited = programPointer program `IntSet.insert` programVisited program}
    go :: Instruction -> Program
    go (Instruction NoOperation _ _) = nextProgram {programPointer = (succ . programPointer) program}
    go (Instruction Jump Plus n) = nextProgram {programPointer = ((+) n . programPointer) program}
    go (Instruction Jump Minus n) = nextProgram {programPointer = (subtract n . programPointer) program}
    go (Instruction Accumulator Plus n) =
      nextProgram
        { programPointer = (succ . programPointer) nextProgram,
          programAcc = ((+) n . programAcc) nextProgram
        }
    go (Instruction Accumulator Minus n) =
      nextProgram
        { programPointer = (succ . programPointer) nextProgram,
          programAcc = (subtract n . programAcc) nextProgram
        }
