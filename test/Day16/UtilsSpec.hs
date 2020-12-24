module Day16.UtilsSpec (spec) where

import Data.Foldable (for_)
import qualified Data.IntMap.Strict as IntMap
import Day16.Utils (intParser, toIntMap)
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec = parallel $ do
  describe "intParser" $ do
    let cases = [("0234", 234), ("1", 1)]
    let test (input, expected) = it ("is " ++ show expected ++ " given " ++ input) $ do
          parse intParser "" input `shouldBe` Right expected

    for_ cases test

  describe "toIntMap" $ do
    let test (input, expected) = it ("is 0 indexed IntMap given " ++ show input) $ do
          toIntMap input `shouldBe` expected

    for_
      [ ("hello", IntMap.fromList [(0, 'h'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o')]),
        ("world", IntMap.fromList [(0, 'w'), (1, 'o'), (2, 'r'), (3, 'l'), (4, 'd')]),
        ("", IntMap.fromList [])
      ]
      test

    for_
      [ ([123, 234, 345] :: [Int], IntMap.fromList [(0, 123), (1, 234), (2, 345)]),
        ([], IntMap.fromList [])
      ]
      test
