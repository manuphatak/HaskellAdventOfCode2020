module Practice.ArrowCircuitSpec where

import Practice.ArrowCircuit
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "runCircuit" $ do
    it "creates running total of all numbers passed as inputs" $ do
      runCircuit total [1, 0, 1, 0, 0, 2] `shouldBe` [1 :: Int, 1, 2, 2, 2, 4]

    it "can calculate mean" $ do
      runCircuit mean1 [0, 10, 7, 8] `shouldBe` [0.0 :: Double, 5.0, 5.666666666666667, 6.25]

    it "can calculate mean" $ do
      runCircuit mean2 [0, 10, 7, 8] `shouldBe` [0.0 :: Double, 5.0, 5.666666666666667, 6.25]
