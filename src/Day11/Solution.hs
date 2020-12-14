module Day11.Solution where

import Advent.Utils (fromRightOrShowError, occurrences)
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences OccupiedSeat . runSimulation nextSeatRulesFromAdjacentSeats . fromRightOrShowError . parseWaitingArea

part2 :: String -> String
part2 = show . occurrences OccupiedSeat . runSimulation nextSeatRulesFromFirstVisible . fromRightOrShowError . parseWaitingArea

type NextSeatRules = WaitingArea -> Point -> Token -> Token

runSimulation :: NextSeatRules -> WaitingArea -> WaitingArea
runSimulation nextSeatRules waitingArea
  | waitingArea == nextWaitingArea = nextWaitingArea
  | otherwise = runSimulation nextSeatRules nextWaitingArea
  where
    nextWaitingArea :: WaitingArea
    nextWaitingArea = Map.mapWithKey (nextSeatRules waitingArea) waitingArea

nextSeatRulesFromAdjacentSeats :: NextSeatRules
nextSeatRulesFromAdjacentSeats waitingArea point = go
  where
    go :: Token -> Token
    go EmptySeat | OccupiedSeat `notElem` adjacentSeats point = OccupiedSeat
    go OccupiedSeat | (>= 4) . occurrences OccupiedSeat $ adjacentSeats point = EmptySeat
    go token = token

    adjacentSeats :: Point -> [Token]
    adjacentSeats (Point x y) =
      mapMaybe (`Map.lookup` waitingArea) $
        [ Point (x + i) (y + j)
          | i <- [-1 .. 1],
            j <- [-1 .. 1],
            (i, j) /= (0, 0)
        ]

nextSeatRulesFromFirstVisible :: NextSeatRules
nextSeatRulesFromFirstVisible waitingArea point = go
  where
    go :: Token -> Token
    go EmptySeat | OccupiedSeat `notElem` adjacentSeats point = OccupiedSeat
    go OccupiedSeat | (>= 5) . occurrences OccupiedSeat $ adjacentSeats point = EmptySeat
    go token = token

    adjacentSeats :: Point -> [Token]
    adjacentSeats point =
      catMaybes $
        [ nextToken point dx dy
          | dx <- [-1 .. 1],
            dy <- [-1 .. 1],
            (dx, dy) /= (0, 0)
        ]
    nextToken :: Point -> Int -> Int -> Maybe Token
    nextToken (Point x y) dx dy = nextPoint `Map.lookup` waitingArea >>= go'
      where
        nextPoint :: Point
        nextPoint = Point (x + dx) (y + dy)

        go' :: Token -> Maybe Token
        go' Floor = nextToken nextPoint dx dy
        go' token = pure token

type WaitingArea = Map.Map Point Token

data Point = Point Int Int deriving (Show, Eq, Ord)

data Token = Floor | EmptySeat | OccupiedSeat deriving (Show, Eq)

parseWaitingArea :: String -> Either ParseError WaitingArea
parseWaitingArea = parse waitingAreaParser ""

waitingAreaParser :: Parsec String () WaitingArea
waitingAreaParser = asWaitingArea . zip [0 ..] <$> (zip [0 ..] <$> rowParser) `sepEndBy1` newline
  where
    asWaitingArea :: [(Int, [(Int, Token)])] -> WaitingArea
    asWaitingArea = Map.fromList . asPoints
    asPoints :: [(Int, [(Int, Token)])] -> [(Point, Token)]
    asPoints = concatMap (uncurry (map . first . Point))

rowParser :: Parsec String () [Token]
rowParser = many1 tokenParser

tokenParser :: Parsec String () Token
tokenParser = asToken <$> oneOf "L#."
  where
    asToken :: Char -> Token
    asToken 'L' = EmptySeat
    asToken '#' = OccupiedSeat
    asToken _ = Floor
