module Day02.UtilsSpec (spec) where

import Data.Foldable (for_)
import Day02.Utils (xor)
import Test.Hspec

spec :: Spec
spec =
  parallel $
    describe "xor" $
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
