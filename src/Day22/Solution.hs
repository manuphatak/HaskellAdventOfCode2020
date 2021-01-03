module Day22.Solution where

import Advent.Parser
import Advent.Utils
import Data.Foldable
import Data.Function
import Data.Sequence hiding (length, zipWith)
import qualified Data.Set as Set
import Text.Parsec hiding (Empty)
import Prelude hiding (reverse)

part1 :: String -> String
part1 = show . winningScore . combat . fromRightOrShowError . parseGame

part2 :: String -> String
part2 = show . winningScore . fst . recursiveCombat 1 1 Set.empty . fromRightOrShowError . parseGame

type Game = (Seq Int, Seq Int)

parseGame :: String -> Either ParseError Game
parseGame = parse gameParser ""
  where
    gameParser :: Parsec String () Game
    gameParser = (,) <$> playerParser "1" <*> (newline *> playerParser "2" <* eof)

    playerParser :: String -> Parsec String () (Seq Int)
    playerParser n = string ("Player " ++ n ++ ":") *> newline *> (fromList <$> intParser `sepEndBy1` newline)

combat :: Game -> Game
combat (x :<| xs, y :<| ys)
  | x > y = combat (xs |> x |> y, ys)
  | y > x = combat (xs, ys |> y |> x)
  | otherwise = combat (xs |> x, ys |> y)
combat g = g

type History = Set.Set Game

-- | otherwise = trace ("round " ++ show r ++ " game " ++ show i) $ traceShow game $ go game
recursiveCombat :: Int -> Int -> History -> Game -> (Game, History)
recursiveCombat i r history game
  | game `Set.member` history = ((fst game, Empty), nextHistory)
  | otherwise = go game
  where
    go :: Game -> (Game, History)
    go (x :<| xs, y :<| ys)
      | x <= length xs && y <= length ys =
        recursiveCombat (succ i) 1 nextHistory (xs, ys)
          & (\(g, h) -> recursiveCombat i (succ r) h (nextGame x xs y ys (gameWinner g)))
      -- & gameWinner
      -- & nextGame x xs y ys
      -- & recursiveCombat i (succ r) nextHistory
      | otherwise =
        x `compare` y
          & nextGame x xs y ys
          & recursiveCombat i (succ r) nextHistory
    go g = (g, nextHistory)

    nextHistory :: Set.Set (Seq Int, Seq Int)
    nextHistory = Set.insert game history

    nextGame :: Int -> Seq Int -> Int -> Seq Int -> Ordering -> Game
    nextGame x xs y ys EQ = (xs |> x, ys |> y)
    nextGame x xs y ys LT = (xs, ys |> y |> x)
    nextGame x xs y ys GT = (xs |> x |> y, ys)

gameWinner :: Game -> Ordering
gameWinner (_, Empty) = GT
gameWinner (Empty, _) = LT
gameWinner _ = EQ

winningScore :: Game -> Int
winningScore (Empty, scores) = scores & reverse & toList & zipWith (*) [1 ..] & sum
winningScore (scores, Empty) = scores & reverse & toList & zipWith (*) [1 ..] & sum
winningScore _ = undefined
