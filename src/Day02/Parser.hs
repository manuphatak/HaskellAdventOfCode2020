module Day02.Parser (parseLine, Password, PasswordPolicy (..)) where

import Advent.Utils (rightToMaybe)
import Text.Parsec

type Password = String

data PasswordPolicy = PasswordPolicy Int Int Char deriving (Show, Eq)

parseLine :: String -> Maybe (PasswordPolicy, String)
parseLine = rightToMaybe . parse lineParser ""
  where
    lineParser :: Parsec String st (PasswordPolicy, Password)
    lineParser = (,) <$> passwordPolicyParser <*> (string ": " *> passwordParser <* eof)

passwordPolicyParser :: Parsec String st PasswordPolicy
passwordPolicyParser = PasswordPolicy <$> intParser <*> (char '-' *> intParser) <*> (space *> letter)

intParser :: Parsec String st Int
intParser = read <$> many1 digit

passwordParser :: Parsec String st Password
passwordParser = many1 letter
