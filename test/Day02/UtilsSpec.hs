module Day02.UtilsSpec (spec) where

import Data.Foldable (for_)
import Day02.Utils (isBetween, occurrences, rightToMaybe, xor)
import Test.Hspec

spec :: Spec
spec = do
  describe "occurrences" $ do
    it "finds the number of occurrences in a list" $
      occurrences 'b' "abcdefabc" `shouldBe` 2
    it "is 0 on an empty list" $
      occurrences 42 ([] :: [Int]) `shouldBe` 0
  describe "isBetween" $ do
    context "given a range of 1 and 13" $
      let lower = 1 :: Int
          upper = 13 :: Int
          cases =
            [ (0, False),
              (1, True),
              (7, True),
              (13, True),
              (15, False)
            ]
          test (target, expected) =
            it ("is " ++ show expected ++ " for " ++ show target) $
              isBetween lower upper target `shouldBe` expected
       in for_ cases test

  describe "rightToMaybe" $ do
    it "is 'Just a value' when given a 'Right value'" $
      rightToMaybe (Right 100 :: Either String Int) `shouldBe` Just 100
    it "is 'Nothing' when given a 'Left value'" $
      rightToMaybe (Left "Error" :: Either String Int) `shouldBe` Nothing
  describe "xor" $ do
    let cases =
          [ (True, True, False),
            (False, False, False),
            (True, False, True),
            (False, True, True)
          ]
        test (a, b, expected) =
          it ("is " ++ show expected ++ " for " ++ show (a, b)) $
            a `xor` b `shouldBe` expected
     in for_ cases test
