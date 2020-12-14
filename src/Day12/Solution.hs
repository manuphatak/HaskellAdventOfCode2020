module Day12.Solution where

import Advent.Utils (fromRightOrShowError, readInt)
import Debug.Trace
import Text.Parsec
  ( ParseError,
    Parsec,
    digit,
    endOfLine,
    many,
    oneOf,
    parse,
    sepEndBy1,
  )

part1 :: String -> String
part1 = show . manhattanDistance . getPosition . run initialState . fromRightOrShowError . parseInstructions

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

data Direction = North | East | South | West deriving (Enum, Bounded, Eq, Show)

data Point = Point Int Int deriving (Eq, Show)

data State = State {getHeading :: Direction, getPosition :: Point} deriving (Show)

initialState :: State
initialState = State East (Point 0 0)

run :: State -> [Instruction] -> State
run = foldl go
  where
    go :: State -> Instruction -> State
    go state (R, 90) = state {getHeading = (next . getHeading) state}
    go state (R, 180) = state {getHeading = (next . next . getHeading) state}
    go state (R, 270) = state {getHeading = (next . next . next . getHeading) state}
    go state (L, 90) = state {getHeading = (prev . getHeading) state}
    go state (L, 180) = state {getHeading = (prev . prev . getHeading) state}
    go state (L, 270) = state {getHeading = (prev . prev . prev . getHeading) state}
    go (State heading point) (F, value) = State heading (nextPoint heading point value)
    go (State heading point) (N, value) = State heading (nextPoint North point value)
    go (State heading point) (E, value) = State heading (nextPoint East point value)
    go (State heading point) (S, value) = State heading (nextPoint South point value)
    go (State heading point) (W, value) = State heading (nextPoint West point value)
    go _ _ = error "unknown"

    nextPoint :: Direction -> Point -> Value -> Point
    nextPoint direction (Point x y) value = traceShowId $ case direction of
      North -> Point x (y + value)
      East -> Point (x + value) y
      South -> Point x (y - value)
      West -> Point (x - value) y

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y

next :: (Eq p, Bounded p, Enum p) => p -> p
next e
  | e == maxBound = minBound
  | otherwise = succ e

prev :: (Eq p, Bounded p, Enum p) => p -> p
prev e
  | e == minBound = maxBound
  | otherwise = pred e
