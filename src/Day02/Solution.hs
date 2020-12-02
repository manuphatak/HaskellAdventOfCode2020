module Day02.Solution (part1, part2, PasswordPolicy (..), parseLine, lineIsValidV1, lineIsValidV2) where

import Data.Maybe (fromJust)
import Text.Parsec

part1 :: String -> String
part1 = show . occurrences True . fromJust . mapM lineIsValidV1 . lines

part2 :: String -> String
part2 = show . occurrences True . fromJust . mapM lineIsValidV2 . lines

type Password = String

data PasswordPolicy = PasswordPolicy Int Int Char deriving (Show, Eq)

lineIsValidV1 :: String -> Maybe Bool
lineIsValidV1 = fmap (uncurry isValid) . parseLine
  where
    isValid :: PasswordPolicy -> Password -> Bool
    isValid (PasswordPolicy l u c) = isBetween l u . occurrences c

lineIsValidV2 :: String -> Maybe Bool
lineIsValidV2 = fmap (uncurry isValid) . parseLine
  where
    isValid :: PasswordPolicy -> Password -> Bool
    isValid (PasswordPolicy a b expected) password = password !! pred a == expected `xor` password !! pred b == expected

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

occurrences :: Eq a => a -> [a] -> Int
occurrences target = length . filter (target ==)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween l u target = target >= l && target <= u

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

infix 3 `xor`