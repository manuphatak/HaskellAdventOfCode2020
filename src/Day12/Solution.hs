{-# LANGUAGE TypeApplications #-}

module Day12.Solution
  ( Heading (..),
    Instruction (..),
    RotateDirection (..),
    Waypoint (..),
    manhattanDistance,
    parseInstructions,
    part1,
    part2,
    run,
  )
where

import Advent.Parser (intParser)
import Advent.Utils (fromRightOrShowError)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Foldable (Foldable (foldl'))
import Text.Parsec hiding (State)

part1 :: String -> String
part1 = show . manhattanDistance . fst . run @Heading . fromRightOrShowError . parseInstructions

part2 :: String -> String
part2 = show . manhattanDistance . fst . run @Waypoint . fromRightOrShowError . parseInstructions

data Heading = North | East | South | West deriving (Show, Eq, Enum, Bounded)

data RotateDirection = L | R deriving (Show, Eq)

rotate :: RotateDirection -> Heading -> Heading
rotate R = cycleSucc
rotate L = cyclePred

data Instruction = MoveAction Heading Int | RotateAction [RotateDirection] | ForwardAction Int deriving (Show, Eq)

type Point = (Int, Int)

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y

move :: Heading -> Int -> Point -> Point
move North = second . (+)
move South = second . subtract
move East = first . (+)
move West = first . subtract

type State a = (Point, a)

class StateReducer a where
  initial :: State a
  handleMove :: Heading -> Int -> State a -> State a
  handleRotate :: [RotateDirection] -> State a -> State a
  handleForward :: Int -> State a -> State a

  reducer :: Instruction -> State a -> State a
  reducer (MoveAction heading value) = handleMove heading value
  reducer (RotateAction rotations) = handleRotate rotations
  reducer (ForwardAction value) = handleForward value

instance StateReducer Heading where
  initial = ((0, 0), East)
  handleMove direction = first . move direction
  handleRotate = second . flip (foldr rotate)
  handleForward value state = handleMove (snd state) value state

newtype Waypoint = Waypoint {getWaypoint :: Point} deriving (Show, Eq)

mapWaypoint :: (Point -> Point) -> Waypoint -> Waypoint
mapWaypoint fn = Waypoint . fn . getWaypoint

rotateWaypoint :: RotateDirection -> Waypoint -> Waypoint
rotateWaypoint R (Waypoint (x, y)) = Waypoint (y, - x)
rotateWaypoint L (Waypoint (x, y)) = Waypoint (- y, x)

add :: Point -> Waypoint -> Point
(x, y) `add` (Waypoint (a, b)) = (a + x, b + y)

instance StateReducer Waypoint where
  initial = ((0, 0), Waypoint (10, 1))
  handleMove direction value = second . mapWaypoint $ move direction value
  handleRotate = second . flip (foldr rotateWaypoint)
  handleForward 0 state = state
  handleForward i (pos, waypoint) = handleForward (pred i) (pos `add` waypoint, waypoint)

run :: StateReducer a => [Instruction] -> State a
run = foldl' (flip reducer) initial

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instructionParser `sepEndBy1` newline) ""
  where
    instructionParser :: Parsec String () Instruction
    instructionParser = choice [try moveActionParser, try rotateActionParser, try forwardActionParser]

    moveActionParser :: Parsec String () Instruction
    moveActionParser = MoveAction <$> headingParser <*> intParser

    headingParser :: Parsec String () Heading
    headingParser = asHeading <$> oneOf ['N', 'E', 'S', 'W']

    rotateActionParser :: Parsec String () Instruction
    rotateActionParser = do
      direction <- asRotateDirection <$> oneOf ['R', 'L']
      count <- (`div` 90) <$> intParser

      pure . RotateAction $ replicate count direction

    forwardActionParser :: Parsec String () Instruction
    forwardActionParser = ForwardAction <$> (char 'F' *> intParser)

    asHeading :: Char -> Heading
    asHeading 'N' = North
    asHeading 'E' = East
    asHeading 'S' = South
    asHeading 'W' = West
    asHeading _ = undefined

    asRotateDirection :: Char -> RotateDirection
    asRotateDirection 'L' = L
    asRotateDirection 'R' = R
    asRotateDirection _ = undefined

cycleSucc :: (Eq p, Bounded p, Enum p) => p -> p
cycleSucc e
  | e == maxBound = minBound
  | otherwise = succ e

cyclePred :: (Eq p, Bounded p, Enum p) => p -> p
cyclePred e
  | e == minBound = maxBound
  | otherwise = pred e
