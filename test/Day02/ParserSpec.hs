module Day02.ParserSpec (spec) where

import Data.Foldable (for_)
import Day02.Parser (PasswordPolicy (..), parseLine)
import Test.Hspec

spec :: Spec
spec =
  parallel $
    describe "parseLine" $
      let cases =
            [ ("1-3 a: abcde", Just (PasswordPolicy 1 3 'a', "abcde")),
              ("1-3 b: cdefg", Just (PasswordPolicy 1 3 'b', "cdefg")),
              ("2-9 c: ccccccccc", Just (PasswordPolicy 2 9 'c', "ccccccccc"))
            ]
          test (input, expected) =
            it ("parses given a line " ++ input) $
              parseLine input `shouldBe` expected
       in for_ cases test
