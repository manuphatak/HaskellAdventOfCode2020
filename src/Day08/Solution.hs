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
    fixedInstructions,
    fixProgram,
  )
where

import Advent.Utils (fromLeft', fromRight'', readInt)
import Data.Either (isRight)
import qualified Data.IntSet as IntSet
import Text.Parsec

part1 :: String -> String
part1 = show . programAcc . fromLeft' . runProgram initialState . fromRight'' . parseInstructions

part2 :: String -> String
part2 = show . programAcc . fromRight'' . fixProgram initialState . fromRight'' . parseInstructions

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
    readOperation _ = error "this should never happen"

signParser :: Parsec String () Sign
signParser = readSign <$> choice [char '+', char '-']
  where
    readSign '+' = Plus
    readSign '-' = Minus
    readSign _ = error "this should never happen"

intParser :: Parsec String () Int
intParser = readInt <$> many1 digit

data Program = Program {programAcc :: Int, programPointer :: Int, programVisited :: IntSet.IntSet} deriving (Show, Eq)

initialState :: Program
initialState = Program {programAcc = 0, programPointer = 0, programVisited = IntSet.empty}

runProgram :: Program -> [Instruction] -> Either Program Program
runProgram program instructions
  | programPointer program `IntSet.member` programVisited program = Left program
  | programPointer program == length instructions = Right program
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

fixProgram :: Program -> [Instruction] -> Either Program Program
fixProgram program = head . filter isRight . map (runProgram program) . fixedInstructions

fixedInstructions :: [Instruction] -> [[Instruction]]
fixedInstructions instructions = [adjust i swappedInstruction instructions | i <- [0 .. (length instructions)]]
  where
    swappedInstruction :: Instruction -> Instruction
    swappedInstruction (Instruction NoOperation sign int) = Instruction Jump sign int
    swappedInstruction (Instruction Jump sign int) = Instruction NoOperation sign int
    swappedInstruction instruction = instruction

adjust :: Int -> (a -> a) -> [a] -> [a]
adjust n fn = go 0
  where
    go _ [] = []
    go i (x : xs)
      | i == n = fn x : go (succ i) xs
      | otherwise = x : go (succ i) xs
