module Day16.Utils where

import Advent.Utils (readInt)
import qualified Data.IntMap.Strict as IntMap
import Text.Parsec (Parsec, digit, many1)

intParser :: Parsec String () Int
intParser = readInt <$> many1 digit

toIntMap :: [a] -> IntMap.IntMap a
toIntMap = IntMap.fromList . zip [0 ..]
