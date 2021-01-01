module Advent.Parser where

import Advent.Utils (readInt)
import Text.Parsec (Parsec, digit, many1)

intParser :: Parsec String () Int
intParser = readInt <$> many1 digit
