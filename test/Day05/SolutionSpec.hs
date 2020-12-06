module Day05.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day05.Solution (Seat (..), decode, part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day05/input.txt"
    part1 input `shouldBe` "998"
  it "solves Part 2" $ do
    input <- readFile "./test/Day05/input.txt"
    part2 input `shouldBe` "676"
  describe "decode" $ do
    let cases =
          [ ("BFFFBBFRRR", Seat {seatRow = 70, seatCol = 7, seatId = 567}),
            ("FFFBBBFRRR", Seat {seatRow = 14, seatCol = 7, seatId = 119}),
            ("BBFFBBFRLL", Seat {seatRow = 102, seatCol = 4, seatId = 820})
          ]
        test (input, expected) =
          it ("converts " ++ input ++ " into " ++ show expected) $
            decode input `shouldBe` expected
     in for_ cases test
