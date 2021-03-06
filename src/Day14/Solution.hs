module Day14.Solution where

import Advent.Parser (intParser)
import Advent.Utils (fromRightOrShowError)
import Data.Bits (Bits (clearBit, setBit))
import Data.Foldable (Foldable (foldl'))
import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import Text.Parsec hiding (State)

part1 :: String -> String
part1 = show . sum . stateMemory . runProgram reducerV1 . fromRightOrShowError . parseInstructions

part2 :: String -> String
part2 = show . sum . stateMemory . runProgram reducerV2 . fromRightOrShowError . parseInstructions

data Mask = X | B Bool deriving (Show, Eq)

data Instruction = SetMemory Int Int | SetMask [Mask] deriving (Show, Eq)

type MaskBits = [(Int, Mask)]

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instructionParser `sepEndBy1` newline) ""
  where
    instructionParser :: Parsec String () Instruction
    instructionParser = choice [try maskParser, memoryParser]

    maskParser :: Parsec String () Instruction
    maskParser = SetMask <$> (string "mask = " *> count 36 maskBitParser)

    memoryParser :: Parsec String () Instruction
    memoryParser = SetMemory <$> (string "mem" *> between (char '[') (char ']') intParser <* spaces <* char '=' <* spaces) <*> intParser

    maskBitParser :: Parsec String () Mask
    maskBitParser = fromMask <$> oneOf ['X', '1', '0']
      where
        fromMask :: Char -> Mask
        fromMask '1' = B True
        fromMask '0' = B False
        fromMask _ = X

data State = State {stateMask :: MaskBits, stateMemory :: IntMap.IntMap Int} deriving (Show)

type Reducer = State -> Instruction -> State

reducerV1 :: Reducer
reducerV1 state (SetMask mask) = state {stateMask = zip [0 ..] . reverse $ mask}
reducerV1 state (SetMemory address value) = state {stateMemory = IntMap.insert address (nextValue value) . stateMemory $ state}
  where
    nextValue :: Int -> Int
    nextValue b = foldr applyMask b [(c, d) | (c, B d) <- stateMask state]

    applyMask :: (Int, Bool) -> Int -> Int
    applyMask (i, True) x = x `setBit` i
    applyMask (i, False) x = x `clearBit` i

reducerV2 :: Reducer
reducerV2 state (SetMask mask) = state {stateMask = zip [0 ..] . reverse $ mask}
reducerV2 state (SetMemory address value) = foldr (setMemory value) state (nextAddresses address)
  where
    nextAddresses :: Int -> [Int]
    nextAddresses = flip computedAddresses (stateMask state)

runProgram :: Reducer -> [Instruction] -> State
runProgram reducer = foldl' reducer initialState
  where
    initialState :: State
    initialState = State [] IntMap.empty

computedAddresses :: Int -> MaskBits -> [Int]
computedAddresses = foldr go . pure
  where
    go :: (Int, Mask) -> [Int] -> [Int]
    go (i, B True) = map (`setBit` i)
    go (_, B False) = id
    go (i, X) = nub . concatMap (\y -> [(`setBit` i), (`clearBit` i)] <*> pure y)

setMemory :: Int -> Int -> State -> State
setMemory value key state = state {stateMemory = IntMap.insert key value $ stateMemory state}
