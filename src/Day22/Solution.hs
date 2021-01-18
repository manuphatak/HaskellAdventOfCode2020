module Day22.Solution
  ( Player (..),
    parseDecks,
    part1,
    part2,
    playGameWith,
    recursiveCombat,
    simpleCombat,
    winningScore,
  )
where

import Advent.Parser (intParser)
import Advent.Utils (fromRightOrShowError)
import Control.Monad (guard)
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec hiding (Empty)

part1 :: String -> String
part1 = show . winningScore . uncurry (playGameWith simpleCombat) . fromRightOrShowError . parseDecks

part2 :: String -> String
part2 = show . winningScore . uncurry (playGameWith recursiveCombat) . fromRightOrShowError . parseDecks

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

type History = Set (Deck, Deck)

type CombatHandler = (Deck -> Deck -> Maybe Player)

simpleCombat :: CombatHandler
simpleCombat = const . const Nothing

recursiveCombat :: CombatHandler
recursiveCombat (x :<| xs) (y :<| ys) = do
  xs' <- takeExactly x xs
  ys' <- takeExactly y ys
  pure . fst $ playGameWith recursiveCombat xs' ys'
recursiveCombat _ _ = undefined

takeExactly :: Int -> Seq Int -> Maybe Deck
takeExactly n xs = Seq.take n xs <$ guard (Seq.length xs >= n)

playGameWith :: CombatHandler -> Deck -> Deck -> (Player, Deck)
playGameWith fn = go Set.empty
  where
    go :: History -> Deck -> Deck -> (Player, Deck)
    go history xs ys
      | (xs, ys) `Set.member` history = (Player1, xs)
      | otherwise = switch history xs ys

    switch :: History -> Deck -> Deck -> (Player, Deck)
    switch _ d Empty = (Player1, d)
    switch _ Empty d = (Player2, d)
    switch history xss@(x :<| xs) yss@(y :<| ys) =
      let winner = case fn xss yss of
            Nothing -> if x > y then Player1 else Player2
            Just p -> p
       in case winner of
            Player1 -> go nextHistory (xs :|> x :|> y) ys
            Player2 -> go nextHistory xs (ys :|> y :|> x)
      where
        nextHistory = Set.insert (xss, yss) history

winningScore :: (a, Deck) -> Int
winningScore (_, deck) = deck & Seq.reverse & toList & zipWith (*) [1 ..] & sum
