module Day02.Solution (part1, part2, PasswordPolicy (..), parseLine, isValid) where

import Data.Maybe (fromJust)
import Text.Parsec

part1 :: String -> String
part1 = show . countValid . fromJust . mapM lineIsValid . lines
  where
    lineIsValid :: String -> Maybe Bool
    lineIsValid = fmap (uncurry isValid) . parseLine
    countValid :: [Bool] -> Int
    countValid = length . filter (== True)

part2 :: String -> String
part2 = id

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

isValid :: PasswordPolicy -> Password -> Bool
isValid (PasswordPolicy l u c) password = isBetween l u $ occurrences c password

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween l u target = target >= l && target <= u

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
