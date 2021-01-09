module Day23.CircularListSpec (spec) where

import Control.Exception
import Data.Function
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Day23.CircularList
import Test.Hspec

spec :: Spec
spec = parallel $ do
  let exampleList = [3, 8, 9, 1, 2, 5, 4, 6, 7] :: [Int]
  let exampleCList = fromList exampleList

  describe "fromList" $ do
    it "creates a circular list" $ do
      fromList exampleList `shouldBe` CList (Seq.fromList exampleList, Empty, Empty)

  describe "toList" $ do
    it "creates a list from a circular list" $ do
      toList exampleCList `shouldBe` exampleList

  describe "show" $ do
    it "shows each element in the list" $ do
      show exampleCList `shouldBe` "fromList [3,8,9,1,2,5,4,6,7]"
    it "wraps the circular part" $ do
      (exampleCList & skipR 3 & show) `shouldBe` "fromList [1,2,5,4,6,7,3,8,9]"
    it "is an error when n is negative" $ do
      evaluate (exampleCList & skipR (-2)) `shouldThrow` anyException

  describe "takeR" $ do
    it "takes elements from the list" $ do
      (exampleCList & takeR 3) `shouldBe` [3, 8, 9]
    it "wraps the circular part" $ do
      (exampleCList & skipR 7 & takeR 3) `shouldBe` [6, 7, 3]
    it "is an error when n is negative" $ do
      evaluate (exampleCList & takeR (-2)) `shouldThrow` anyException

  describe "dropR" $ do
    it "drops elements from the list" $ do
      (exampleCList & dropR 3) `shouldBe` fromList [1, 2, 5, 4, 6, 7]
    it "wraps the circular part" $ do
      (exampleCList & skipR 7 & dropR 3) `shouldBe` fromList [8, 9, 1, 2, 5, 4]
    it "is an error when n is negative" $ do
      evaluate (exampleCList & dropR (-2)) `shouldThrow` anyException

  describe "dropR'" $ do
    it "it returns the dropped elements" $ do
      (exampleCList & dropR' 3) `shouldBe` (Seq.fromList [3, 8, 9], fromList [1, 2, 5, 4, 6, 7])
    it "wraps the circular part" $ do
      (exampleCList & skipR 7 & dropR' 3) `shouldBe` (Seq.fromList [6, 7, 3], fromList [8, 9, 1, 2, 5, 4])
    it "is an error when n is negative" $ do
      evaluate (exampleCList & dropR' (-2)) `shouldThrow` anyException

  describe "skipR" $ do
    it "skips elements on the list without removing them" $ do
      (exampleCList & skipR 3) `shouldBe` fromList [1, 2, 5, 4, 6, 7, 3, 8, 9]
    it "wraps the circular part" $ do
      (exampleCList & skipR (3 + length exampleList)) `shouldBe` fromList [1, 2, 5, 4, 6, 7, 3, 8, 9]
    it "is an error when n is negative" $ do
      evaluate (exampleCList & skipR (-2)) `shouldThrow` anyException

  describe "skipL" $ do
    it "skips elements on the list without removing them" $ do
      (exampleCList & skipL 3) `shouldBe` fromList [4, 6, 7, 3, 8, 9, 1, 2, 5]
    it "wraps the circular part" $ do
      (exampleCList & skipL (3 + length exampleList)) `shouldBe` fromList [4, 6, 7, 3, 8, 9, 1, 2, 5]
    it "is an error when n is negative" $ do
      evaluate (exampleCList & skipL (-2)) `shouldThrow` anyException

  describe "skipWhileR" $ do
    it "skips elements on the list without removing them" $ do
      (exampleCList & skipWhileR (/= 1)) `shouldBe` fromList [1, 2, 5, 4, 6, 7, 3, 8, 9]
    it "wraps the circular part" $ do
      (exampleCList & skipR 3 & skipWhileR (/= 3)) `shouldBe` exampleCList

  describe "cons" $ do
    it "inserts a value at the current position" $ do
      (exampleCList & cons 13) `shouldBe` fromList [13, 3, 8, 9, 1, 2, 5, 4, 6, 7]
    it "wraps the circular part" $ do
      (exampleCList & skipL 1 & cons 13) `shouldBe` fromList [13, 7, 3, 8, 9, 1, 2, 5, 4, 6]

  describe "insertMany" $ do
    it "inserts a value at the current position" $ do
      (exampleCList & insertMany [13, 17, 19]) `shouldBe` fromList [13, 17, 19, 3, 8, 9, 1, 2, 5, 4, 6, 7]
    it "wraps the circular part" $ do
      (exampleCList & skipL 1 & insertMany [13, 17, 19]) `shouldBe` fromList [13, 17, 19, 7, 3, 8, 9, 1, 2, 5, 4, 6]

  describe "peek" $ do
    it "shows the current value" $ do
      (exampleCList & peek) `shouldBe` 3

  describe "yankR" $ do
    it "cuts elements from the list" $ do
      (exampleCList & yankR 3) `shouldBe` fromList [1, 2, 5, 4, 6, 7]
    it "wraps the circular part" $ do
      (exampleCList & skipR 7 & yankR 3) `shouldBe` fromList [8, 9, 1, 2, 5, 4]
    it "is an error when n is negative" $ do
      evaluate (exampleCList & yankR (-3)) `shouldThrow` anyException

  describe "putR" $ do
    it "is an identity" $ do
      (exampleCList & putR) `shouldBe` exampleCList
    it "is the duel of a yankR" $ do
      (exampleCList & yankR 3 & putR) `shouldBe` exampleCList
    it "wraps the circular part" $ do
      (exampleCList & skipR 7 & yankR 3 & putR) `shouldBe` fromList [6, 7, 3, 8, 9, 1, 2, 5, 4]
    it "lift and shift sections of the list" $ do
      (exampleCList & skipR 1 & yankR 3 & skipR 1 & putR) `shouldBe` fromList [8, 9, 1, 5, 4, 6, 7, 3, 2]

  describe "sortBy" $ do
    it "sorts elements" $ do
      {- HLINT ignore "Use sort" -}
      (exampleCList & sortBy compare) `shouldBe` fromList [3, 4, 5, 6, 7, 8, 9, 1, 2]
    it "sorts elements" $ do
      (exampleCList & sort) `shouldBe` fromList [3, 4, 5, 6, 7, 8, 9, 1, 2]
    it "sorts elements in reverse" $ do
      (exampleCList & sortBy (flip compare)) `shouldBe` fromList [3, 2, 1, 9, 8, 7, 6, 5, 4]
