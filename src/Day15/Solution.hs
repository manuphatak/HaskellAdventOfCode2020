module Day15.Solution where

import Advent.Utils
import qualified Data.IntMap.Lazy as IntMap
import Text.Parsec hiding (State)

part1 :: String -> String
part1 = show . (!! (2020 -1)) . memoryGame . fromRightOrShowError . parseInts

part2 :: String -> String
part2 = head . lines

parseInts :: String -> Either ParseError [Int]
parseInts = parse (intsParser <* endOfLine) ""
  where
    intsParser :: Parsec String () [Int]
    intsParser = (readInt <$> many1 digit) `sepBy` char ','

type State = IntMap.IntMap Int

memoryGame :: [Int] -> [Int]
memoryGame xs = xs ++ go (length xs, last xs, initialState)
  where
    go :: (Int, Int, State) -> [Int]
    go (turn, prev, state) = do
      let next = maybe 0 (turn -) $ IntMap.lookup prev state
      let nextState = IntMap.insert prev turn state

      next : go (succ turn, next, nextState)

    initialState :: State
    initialState = IntMap.fromList $ zip (init xs) [1 ..]
