module Day22.Solution where

import Advent.Parser
import Advent.Utils
import Data.Foldable
import Data.Function
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec hiding (Empty)

part1 :: String -> String
part1 = show . winningScore . uncurry (playGameWith const) . fromRightOrShowError . parseDecks

part2 :: String -> String
part2 = show . winningScore . uncurry (playGameWith const) . fromRightOrShowError . parseDecks

parseDecks :: String -> Either ParseError (Deck, Deck)
parseDecks = parse gameParser ""
  where
    gameParser :: Parsec String () (Deck, Deck)
    gameParser = (,) <$> playerParser "1" <*> (newline *> playerParser "2" <* eof)

    playerParser :: String -> Parsec String () Deck
    playerParser n = string ("Player " ++ n ++ ":") *> newline *> deckParser

    deckParser :: Parsec String () Deck
    deckParser = Seq.fromList <$> intParser `sepEndBy1` newline

type Deck = Seq Int

data Player = Player1 | Player2 deriving (Show, Eq)

playGameWith :: a -> Deck -> Deck -> (Player, Deck)
playGameWith fn = switch
  where
    switch :: Deck -> Deck -> (Player, Deck)
    switch (x :<| xs) (y :<| ys)
      | x > y = switch (xs :|> x :|> y) ys
      | y > x = switch xs (ys :|> y :|> x)
      | otherwise = switch (xs :|> x) (ys :|> y)
    switch d Empty = (Player1, d)
    switch Empty d = (Player2, d)

type History = Set (Deck, Deck)

-- | otherwise = trace ("round " ++ show r ++ " game " ++ show i) $ traceShow game $ go game
-- recursiveCombat :: Int -> Int -> History -> (Deck, Deck) -> ((Deck, Deck), History)
-- recursiveCombat i r history game
--   | game `Set.member` history = ((fst game, Empty), nextHistory)
--   | otherwise = go game
--   where
--     go :: (Deck, Deck) -> ((Deck, Deck), History)
--     go (x :<| xs, y :<| ys)
--       | x <= length xs && y <= length ys =
--         recursiveCombat (succ i) 1 nextHistory (xs, ys)
--           & (\(g, h) -> recursiveCombat i (succ r) h (nextGame x xs y ys (gameWinner g)))
--       | otherwise =
--         x `compare` y
--           & nextGame x xs y ys
--           & recursiveCombat i (succ r) nextHistory
--     go g = (g, nextHistory)

--     nextHistory :: Set (Seq Int, Seq Int)
--     nextHistory = Set.insert game history

--     nextGame :: Int -> Seq Int -> Int -> Seq Int -> Ordering -> (Deck, Deck)
--     nextGame x xs y ys EQ = (xs :|> x, ys :|> y)
--     nextGame x xs y ys LT = (xs, ys :|> y :|> x)
--     nextGame x xs y ys GT = (xs :|> x :|> y, ys)

winningScore :: (a, Deck) -> Int
winningScore (_, deck) = deck & Seq.reverse & toList & zipWith (*) [1 ..] & sum
