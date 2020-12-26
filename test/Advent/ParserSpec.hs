module Advent.ParserSpec (spec) where

import Advent.Parser (intParser)
import Data.Foldable (for_)
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec = parallel $ do
  describe "intParser" $ do
    let cases = [("0234", 234), ("1", 1)]
    let test (input, expected) = it ("is " ++ show expected ++ " given " ++ input) $ do
          parse intParser "" input `shouldBe` Right expected

    for_ cases test
