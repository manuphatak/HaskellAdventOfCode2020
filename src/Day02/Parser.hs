module Day02.Parser (parseLine, Password, PasswordPolicy (..)) where

import Advent.Parser (intParser)
import Advent.Utils (rightToMaybe)
import Text.Parsec

type Password = String

data PasswordPolicy = PasswordPolicy Int Int Char deriving (Show, Eq)

parseLine :: String -> Maybe (PasswordPolicy, String)
parseLine = rightToMaybe . parse lineParser ""
  where
    lineParser :: Parsec String () (PasswordPolicy, Password)
    lineParser = (,) <$> passwordPolicyParser <*> (string ": " *> passwordParser <* eof)

passwordPolicyParser :: Parsec String () PasswordPolicy
passwordPolicyParser = PasswordPolicy <$> intParser <*> (char '-' *> intParser) <*> (space *> letter)

passwordParser :: Parsec String () Password
passwordParser = many1 letter
