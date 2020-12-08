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

import Advent.Utils (readInt)
import Data.Either (isRight)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Day08.Utils (asIntMap, fromLeftOrError, fromRightOrError')
import Text.Parsec

part1 :: String -> String
part1 = show . programAcc . fromLeftOrError . runProgram initialState . fromRightOrError' . parseInstructions

part2 :: String -> String
part2 = show . programAcc . fromRightOrError' . fixProgram initialState . fromRightOrError' . parseInstructions

data Sign = Plus | Minus deriving (Show, Eq)

data Operation = NoOperation | Accumulator | Jump deriving (Show, Eq)

data Instruction = Instruction Operation Sign Int deriving (Show, Eq)

type Instructions = IntMap.IntMap Instruction

parseInstructions :: String -> Either ParseError Instructions
parseInstructions = parse instructionsParser ""
  where
    instructionsParser :: Parsec String () Instructions
    instructionsParser = asIntMap <$> instructionParser `sepEndBy1` endOfLine

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

runProgram :: Program -> Instructions -> Either Program Program
runProgram program instructions
  | programPointer program `IntSet.member` programVisited program = Left program
  | programPointer program == length instructions = Right program
  | otherwise = runProgram (go (instructions IntMap.! programPointer program)) instructions
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

fixProgram :: Program -> Instructions -> Either Program Program
fixProgram program = head . filter isRight . map (runProgram program) . fixedInstructions

fixedInstructions :: Instructions -> [Instructions]
fixedInstructions instructions =
  [ IntMap.adjust swappedInstruction i instructions
    | i <- [0 .. (length instructions)],
      let (Instruction op _ _) = instructions IntMap.! i,
      op == NoOperation || op == Jump
  ]
  where
    swappedInstruction :: Instruction -> Instruction
    swappedInstruction (Instruction NoOperation sign int) = Instruction Jump sign int
    swappedInstruction (Instruction Jump sign int) = Instruction NoOperation sign int
    swappedInstruction _ = error "this should never happen"
