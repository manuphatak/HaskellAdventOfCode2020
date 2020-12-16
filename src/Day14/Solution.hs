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
part1 = show . sum . stateMemory . runProgram . fromRightOrShowError . parseInstructions

part2 :: String -> String
part2 = head . lines

data Instruction = Memory Int Int | Mask [Maybe Bool] deriving (Show, Eq)

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instructionParser `sepEndBy1` newline) ""
  where
    instructionParser :: Parsec String () Instruction
    instructionParser = choice [try maskParser, memoryParser]

    maskParser :: Parsec String () Instruction
    maskParser = Mask <$> (string "mask = " *> count 36 maskBitParser)

    memoryParser :: Parsec String () Instruction
    memoryParser = Memory <$> (string "mem" *> between (char '[') (char ']') intParser <* spaces <* char '=' <* spaces) <*> intParser

    maskBitParser :: Parsec String () (Maybe Bool)
    maskBitParser = fromMask <$> oneOf ['X', '1', '0']
      where
        fromMask :: Char -> Maybe Bool
        fromMask '1' = Just True
        fromMask '0' = Just False
        fromMask _ = Nothing

    intParser :: Parsec String () Int
    intParser = readInt <$> many1 digit

data State = State {stateMask :: [(Int, Bool)], stateMemory :: IntMap.IntMap Int} deriving (Show)

runProgram :: [Instruction] -> State
runProgram = foldl' go initialState
  where
    initialState :: State
    initialState = State [] IntMap.empty

    go :: State -> Instruction -> State
    go state (Mask mask) =
      state
        { stateMask = catMaybesOnSnd . zip [0 ..] . reverse $ mask
        }
    go state (Memory address value) =
      state
        { stateMemory = IntMap.insert address (setMemory (stateMask state) value) . stateMemory $ state
        }

    setMemory :: [(Int, Bool)] -> Int -> Int
    setMemory ms b = foldr applyMask b ms

    applyMask :: (Int, Bool) -> Int -> Int
    applyMask (i, True) x = x `setBit` i
    applyMask (i, False) x = x `clearBit` i

catMaybesOnSnd :: [(Int, Maybe Bool)] -> [(Int, Bool)]
catMaybesOnSnd = foldr go []
  where
    go :: (Int, Maybe Bool) -> [(Int, Bool)] -> [(Int, Bool)]
    go (_, Nothing) xs = xs
    go (a, Just b) xs = (a, b) : xs
