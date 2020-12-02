module Day02.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day02.Solution (PasswordPolicy (..), isValid, parseLine, part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day02/input.txt"
    part1 input `shouldBe` "378"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day02/input.txt"
    part2 input `shouldBe` "hello santa"
  describe "parseLine" $ do
    let cases =
          [ ("1-3 a: abcde", Just (PasswordPolicy 1 3 'a', "abcde")),
            ("1-3 b: cdefg", Just (PasswordPolicy 1 3 'b', "cdefg")),
            ("2-9 c: ccccccccc", Just (PasswordPolicy 2 9 'c', "ccccccccc"))
          ]
        test (input, expected) = it ("parses given a line " ++ input) $ do
          parseLine input `shouldBe` expected
     in for_ cases test
  describe "isValid" $ do
    let cases =
          [ (PasswordPolicy 1 3 'a', "abcde", True),
            (PasswordPolicy 1 3 'b', "cdefg", False),
            (PasswordPolicy 2 9 'c', "ccccccccc", True)
          ]
        test (policy, password, expected) = context ("given a policy " ++ (show policy) ++ " and a password " ++ password) $ do
          it ("validates as " ++ show expected) $ isValid policy password `shouldBe` expected
     in for_ cases test