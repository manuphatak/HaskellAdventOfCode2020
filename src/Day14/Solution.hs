module Day14.Solution where

import Advent.Utils (fromRightOrShowError, readInt)
import Data.Bits
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Text.Parsec
  ( ParseError,
    Parsec,
    between,
    char,
    choice,
    count,
    digit,
    many1,
    newline,
    oneOf,
    parse,
    sepEndBy1,
    spaces,
    string,
    try,
  )

part1 :: String -> String
part1 = show . sum . stateMemory . runProgram reducerV1 . fromRightOrShowError . parseInstructions

part2 :: String -> String
part2 = head . lines

data Mask a = X | B a deriving (Show, Eq)

data Instruction = SetMemory Int Int | SetMask [Mask Bool] deriving (Show, Eq)

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instructionParser `sepEndBy1` newline) ""
  where
    instructionParser :: Parsec String () Instruction
    instructionParser = choice [try maskParser, memoryParser]

    maskParser :: Parsec String () Instruction
    maskParser = SetMask <$> (string "mask = " *> count 36 maskBitParser)

    memoryParser :: Parsec String () Instruction
    memoryParser = SetMemory <$> (string "mem" *> between (char '[') (char ']') intParser <* spaces <* char '=' <* spaces) <*> intParser

    maskBitParser :: Parsec String () (Mask Bool)
    maskBitParser = fromMask <$> oneOf ['X', '1', '0']
      where
        fromMask :: Char -> Mask Bool
        fromMask '1' = B True
        fromMask '0' = B False
        fromMask _ = X

    intParser :: Parsec String () Int
    intParser = readInt <$> many1 digit

data State = State {stateMask :: [(Int, Bool)], stateMemory :: IntMap.IntMap Int} deriving (Show)

type Reducer = State -> Instruction -> State

reducerV1 :: Reducer
reducerV1 state = go
  where
    go :: Instruction -> State
    go (SetMask mask) = state {stateMask = catBitsOnSnd . zip [0 ..] . reverse $ mask}
    go (SetMemory address value) = state {stateMemory = IntMap.insert address (nextValue value) . stateMemory $ state}

    nextValue :: Int -> Int
    nextValue b = foldr applyMask b (stateMask state)

    applyMask :: (Int, Bool) -> Int -> Int
    applyMask (i, True) x = x `setBit` i
    applyMask (i, False) x = x `clearBit` i

reducerV2 :: Reducer
reducerV2 state = go
  where
    go :: Instruction -> State
    go (SetMask mask) = state {stateMask = catBitsOnSnd . zip [0 ..] . reverse $ mask}
    go (SetMemory _ _) = error "TODO"

runProgram :: Reducer -> [Instruction] -> State
runProgram reducer = foldl' reducer initialState
  where
    initialState :: State
    initialState = State [] IntMap.empty

catBitsOnSnd :: [(Int, Mask Bool)] -> [(Int, Bool)]
catBitsOnSnd = foldr go []
  where
    go :: (Int, Mask Bool) -> [(Int, Bool)] -> [(Int, Bool)]
    go (_, X) xs = xs
    go (a, B b) xs = (a, b) : xs
