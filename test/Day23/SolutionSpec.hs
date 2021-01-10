module Day23.SolutionSpec (spec) where

import Data.Foldable (for_)
import qualified Data.IntMap.Lazy as IntMap
import Day23.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day23/input.txt"
    part1 input `shouldBe` "98752463"

  it "solves Part 2" $ do
    pendingWith "takes about 105 seconds to run"

    input <- readFile "./test/Day23/input.txt"
    part2 input `shouldBe` "2000455861"

  let exampleCircularList = fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]
  describe "fromList" $ do
    it "creates a circular list" $ do
      let expected =
            ( 3,
              IntMap.fromList
                [ (3, 8),
                  (8, 9),
                  (9, 1),
                  (1, 2),
                  (2, 5),
                  (5, 4),
                  (4, 6),
                  (6, 7),
                  (7, 3)
                ]
            )
      exampleCircularList `shouldBe` expected

  describe "toUniqList" $ do
    it "creates a flat list" $ do
      toUniqList exampleCircularList `shouldBe` [3, 8, 9, 1, 2, 5, 4, 6, 7]
    it "can be wrapped" $ do
      (toUniqList . goTo 1) exampleCircularList `shouldBe` [1, 2, 5, 4, 6, 7, 3, 8, 9]

  describe "moves" $ do
    let cases =
          [ (1, [2, 8, 9, 1, 5, 4, 6, 7, 3]),
            (2, [5, 4, 6, 7, 8, 9, 1, 3, 2]),
            (3, [8, 9, 1, 3, 4, 6, 7, 2, 5]),
            (4, [4, 6, 7, 9, 1, 3, 2, 5, 8]),
            (5, [1, 3, 6, 7, 9, 2, 5, 8, 4]),
            (6, [9, 3, 6, 7, 2, 5, 8, 4, 1]),
            (7, [2, 5, 8, 3, 6, 7, 4, 1, 9]),
            (8, [6, 7, 4, 1, 5, 8, 3, 9, 2]),
            (9, [5, 7, 4, 1, 8, 3, 9, 2, 6]),
            (10, [8, 3, 7, 4, 1, 9, 2, 6, 5])
          ]
    let test (input, expected) = it ("walks through move " ++ show input) $ do
          (toUniqList . moves input) exampleCircularList `shouldBe` expected

    for_ cases test

  describe "cupOrder" $ do
    it "is 92658374 after 10 moves" $ do
      (cupOrder . moves 10) exampleCircularList `shouldBe` "92658374"
    it "is 67384529 after 100 moves" $ do
      (cupOrder . moves 100) exampleCircularList `shouldBe` "67384529"

  let exampleCircularListFilled = fillCups [3, 8, 9, 1, 2, 5, 4, 6, 7]

  describe "fillCups" $ do
    it "creates a million cups" $ do
      let expected =
            ( 3,
              IntMap.fromList
                [ (3, 8),
                  (8, 9),
                  (9, 1),
                  (1, 2),
                  (2, 5),
                  (5, 4),
                  (4, 6),
                  (6, 7),
                  (7, 10),
                  (oneMillion, 3)
                ]
            )
      exampleCircularListFilled `shouldBe` expected

  describe "adjacentTo" $ do
    context "when running 10 000 000 rounds" $ do
      it "get's the pair next to 1" $ do
        pendingWith "takes too long to run"

        (adjacentTo 1 . moves (10 * oneMillion)) exampleCircularListFilled `shouldBe` [934001, 159792]
