module Day22.Solution where

import Advent.Parser
import Advent.Utils
import Data.Foldable
import Data.Function
import Data.Sequence hiding (zipWith)
import Text.Parsec hiding (Empty)
import Prelude hiding (reverse)

part1 :: String -> String
part1 = show . winningScore . play . fromRightOrShowError . parseGame

part2 :: String -> String
part2 = head . lines

data Game = Game (Seq Int) (Seq Int) Int deriving (Show, Eq)

parseGame :: String -> Either ParseError Game
parseGame = parse gameParser ""
  where
    gameParser :: Parsec String () Game
    gameParser = Game <$> playerParser "1" <*> (newline *> playerParser "2" <* eof) <*> pure 0

    playerParser :: String -> Parsec String () (Seq Int)
    playerParser n = string ("Player " ++ n ++ ":") *> newline *> (fromList <$> intParser `sepEndBy1` newline)

play :: Game -> Game
play (Game (x :<| xs) (y :<| ys) i)
  | x > y = play $ Game (xs |> x |> y) ys (succ i)
  | y > x = play $ Game xs (ys |> y |> x) (succ i)
  | otherwise = play $ Game (xs |> x) (ys |> y) (succ i)
play game = game

winningScore :: Game -> Int
winningScore (Game Empty scores _) = scores & reverse & toList & zipWith (*) [1 ..] & sum
winningScore (Game scores Empty _) = scores & reverse & toList & zipWith (*) [1 ..] & sum
winningScore _ = undefined
