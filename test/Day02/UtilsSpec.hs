module Day02.UtilsSpec (spec) where

import Data.Foldable (for_)
import Day02.Utils (isBetween, xor)
import Test.Hspec

spec :: Spec
spec = parallel $ do
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
