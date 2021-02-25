module Day15.Solution (memoryGame, part1, part2) where

import Advent.Utils (fromRightOrShowError, readInt)
import Control.Monad (liftM3)
import qualified Data.IntMap.Lazy as IntMap
import Text.Parsec
  ( ParseError,
    Parsec,
    char,
    digit,
    endOfLine,
    many1,
    parse,
    sepBy,
  )

part1 :: String -> String
part1 = show . memoryGame 2020 . fromRightOrShowError . parseInts

part2 :: String -> String
part2 = show . memoryGame 30000000 . fromRightOrShowError . parseInts

parseInts :: String -> Either ParseError [Int]
parseInts = parse (intsParser <* endOfLine) ""
  where
    intsParser :: Parsec String () [Int]
    intsParser = (readInt <$> many1 digit) `sepBy` char ','

type State = IntMap.IntMap Int

memoryGame :: Int -> [Int] -> Int
memoryGame target = liftM3 go length last initialState
  where
    go :: Int -> Int -> State -> Int
    go turn prev state
      | turn == target = prev
      | otherwise = go (succ turn) next nextState
      where
        next = (maybe 0 (turn -) . IntMap.lookup prev) state
        nextState = IntMap.insert prev turn state

    initialState :: [Int] -> State
    initialState = IntMap.fromList . flip zip [1 ..] . init
