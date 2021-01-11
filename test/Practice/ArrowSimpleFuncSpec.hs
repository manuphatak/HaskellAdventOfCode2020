module Practice.ArrowSimpleFuncSpec where

import Practice.ArrowSimpleFunc
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "it fans out and collapses" $ do
    runF h 8 `shouldBe` 29
  it "works with arrow notation" $ do
    runF h' 8 `shouldBe` 29
