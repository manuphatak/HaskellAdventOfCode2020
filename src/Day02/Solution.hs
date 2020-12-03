module Day02.Solution (part1, part2, lineIsValidV1, lineIsValidV2) where

import Data.Maybe (fromJust)
import Data.Utils (occurrences)
import Day02.Parser (Password, PasswordPolicy (..), parseLine)
import Day02.Utils (isBetween, xor)

part1 :: String -> String
part1 = show . occurrences True . fromJust . mapM lineIsValidV1 . lines

part2 :: String -> String
part2 = show . occurrences True . fromJust . mapM lineIsValidV2 . lines

lineIsValidV1 :: String -> Maybe Bool
lineIsValidV1 = fmap (uncurry isValid) . parseLine
  where
    isValid :: PasswordPolicy -> Password -> Bool
    isValid (PasswordPolicy lower upper c) = isBetween lower upper . occurrences c

lineIsValidV2 :: String -> Maybe Bool
lineIsValidV2 = fmap (uncurry isValid) . parseLine
  where
    isValid :: PasswordPolicy -> Password -> Bool
    isValid (PasswordPolicy a b expected) password = password !! pred a == expected `xor` password !! pred b == expected
