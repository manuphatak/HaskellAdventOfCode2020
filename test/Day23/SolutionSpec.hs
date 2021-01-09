module Day23.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day23.CircularList
import Day23.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day23/input.txt"
    part1 input `shouldBe` "98752463"

  xit "solves Part 2" $ do
    input <- readFile "./test/Day23/input.txt"
    part2 input `shouldBe` "hello_santa"

  let exampleCList = fromList [3, 8, 9, 1, 2, 5, 4, 6, 7]
  describe "parseCircularList" $ do
    it "reads a list of int characters" $ do
      parseCircularList "389125467" `shouldBe` exampleCList

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
          (toList . moves input) exampleCList `shouldBe` expected

    for_ cases test

  describe "cupOrder" $ do
    it "is 92658374 after 10 moves" $ do
      (cupOrder . moves 10) exampleCList `shouldBe` "92658374"
    it "is 92658374 after 100 moves" $ do
      (cupOrder . moves 100) exampleCList `shouldBe` "67384529"
