module Practice.FoldableSpec (spec) where

import Data.Monoid (Any (Any, getAny))
import Practice.Foldable (testTree)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  context "given a testTree" $ do
    it "folds with (+)" $ do
      sum testTree `shouldBe` 42
    it "folds with (*)" $ do
      product testTree `shouldBe` 64800
    it "reduces into a single Monoid value" $ do
      (getAny . foldMap (Any . (3 ==))) testTree `shouldBe` True
    it "reduces into a single Monoid value" $ do
      (getAny . foldMap (Any . (15 <=))) testTree `shouldBe` False
