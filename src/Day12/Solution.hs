module Day12.Solution where

import Advent.Utils (fromRightOrShowError, readInt)
import Data.Function
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
part1 = show . manhattanDistance . runV1 . fromRightOrShowError . parseInstructions

part2 :: String -> String
part2 = show . manhattanDistance . runV2 . fromRightOrShowError . parseInstructions

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

data Point = Point Value Value deriving (Eq, Show)

moveY :: Value -> Point -> Point
moveY amount (Point x y) = Point x (y + amount)

getY :: Point -> Value
getY (Point _ y) = y

moveX :: Value -> Point -> Point
moveX amount (Point x y) = Point (x + amount) y

getX :: Point -> Value
getX (Point x _) = x

data StateV1 = StateV1 {sHeadingV1 :: Direction, sPositionV1 :: Point} deriving (Show)

runV1 :: [Instruction] -> Point
runV1 = sPositionV1 . foldl go initialState
  where
    initialState :: StateV1
    initialState = StateV1 East (Point 0 0)

    go :: StateV1 -> Instruction -> StateV1
    go state (R, 90) = state {sHeadingV1 = (next . sHeadingV1) state}
    go state (R, 180) = state {sHeadingV1 = (next . next . sHeadingV1) state}
    go state (R, 270) = state {sHeadingV1 = (next . next . next . sHeadingV1) state}
    go state (L, 90) = state {sHeadingV1 = (prev . sHeadingV1) state}
    go state (L, 180) = state {sHeadingV1 = (prev . prev . sHeadingV1) state}
    go state (L, 270) = state {sHeadingV1 = (prev . prev . prev . sHeadingV1) state}
    go state (F, value) = state {sPositionV1 = nextPoint (sHeadingV1 state) (sPositionV1 state) value}
    go state (N, value) = state {sPositionV1 = nextPoint North (sPositionV1 state) value}
    go state (E, value) = state {sPositionV1 = nextPoint East (sPositionV1 state) value}
    go state (S, value) = state {sPositionV1 = nextPoint South (sPositionV1 state) value}
    go state (W, value) = state {sPositionV1 = nextPoint West (sPositionV1 state) value}
    go _ _ = error "Unknown action"

    nextPoint :: Direction -> Point -> Value -> Point
    nextPoint direction point value = case direction of
      North -> moveY value point
      East -> moveX value point
      South -> moveY (- value) point
      West -> moveX (- value) point

    next :: (Eq p, Bounded p, Enum p) => p -> p
    next e
      | e == maxBound = minBound
      | otherwise = succ e

    prev :: (Eq p, Bounded p, Enum p) => p -> p
    prev e
      | e == minBound = maxBound
      | otherwise = pred e

data StateV2 = StateV2 {sWaypointV2 :: Point, sPositionV2 :: Point} deriving (Show)

runV2 :: [Instruction] -> Point
runV2 = sPositionV2 . foldl go initialState
  where
    initialState :: StateV2
    initialState = StateV2 (Point 10 1) (Point 0 0)

    go :: StateV2 -> Instruction -> StateV2
    go state (R, 90) = state {sWaypointV2 = rotateRight . sWaypointV2 $ state}
    go state (R, 180) = state {sWaypointV2 = rotateRight . rotateRight . sWaypointV2 $ state}
    go state (R, 270) = state {sWaypointV2 = rotateRight . rotateRight . rotateRight . sWaypointV2 $ state}
    go state (L, 90) = state {sWaypointV2 = rotateLeft . sWaypointV2 $ state}
    go state (L, 180) = state {sWaypointV2 = rotateLeft . rotateLeft . sWaypointV2 $ state}
    go state (L, 270) = state {sWaypointV2 = rotateLeft . rotateLeft . rotateLeft . sWaypointV2 $ state}
    go state (F, value) =
      state
        { sPositionV2 =
            state
              & sPositionV2
              & moveY ((* value) . getY . sWaypointV2 $ state)
              & moveX ((* value) . getX . sWaypointV2 $ state)
        }
    go state (N, value) = state {sWaypointV2 = moveY value . sWaypointV2 $ state}
    go state (E, value) = state {sWaypointV2 = moveX value . sWaypointV2 $ state}
    go state (S, value) = state {sWaypointV2 = moveY (- value) . sWaypointV2 $ state}
    go state (W, value) = state {sWaypointV2 = moveX (- value) . sWaypointV2 $ state}
    go _ _ = error "Unknown action"

    rotateRight :: Point -> Point
    rotateRight (Point x y) = Point y (- x)

    rotateLeft :: Point -> Point
    rotateLeft (Point x y) = Point (- y) x

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y
