module Day15.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day15.Solution (memoryGame, part1, part2)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day15/input.txt"
    part1 input `shouldBe` "694"
  -- TODO: this runs in ~40 seconds with production optimizations
  xit "solves Part 2" $ do
    input <- readFile "./test/Day15/input.txt"
    part2 input `shouldBe` "21768614"
  describe "memoryGame" $ do
    context "when looking at the 2020th result" $ do
      let n = 2020
      let cases =
            [ ([0, 3, 6], 436),
              ([1, 3, 2], 1),
              ([2, 1, 3], 10),
              ([1, 2, 3], 27),
              ([2, 3, 1], 78),
              ([3, 2, 1], 438),
              ([3, 1, 2], 1836)
            ]
      let test (input, expected) = it ("is " ++ show expected ++ " for input " ++ show input) $ do
            memoryGame n input `shouldBe` expected

      for_ cases test

    -- TODO: figure out how to make this not take 10 minutes
    xcontext "when looking at the 30000000th result" $ do
      let n = 30000000
      let cases =
            [ ([0, 3, 6], 175594),
              ([1, 3, 2], 2578),
              ([2, 1, 3], 3544142),
              ([1, 2, 3], 261214),
              ([2, 3, 1], 6895259),
              ([3, 2, 1], 18),
              ([3, 1, 2], 362)
            ]
      let test (input, expected) = it ("is " ++ show expected ++ " for input " ++ show input) $ do
            memoryGame n input `shouldBe` expected

      for_ cases test

-- 300
-- Finished in 0.0017 seconds
-- Finished in 0.0011 seconds

-- 3000
-- Finished in 0.0061 seconds
-- Finished in 0.0052 seconds

-- 30000
-- Finished in 0.1399 seconds
-- Finished in 0.1418 seconds

-- 300000
-- Finished in 1.7987 seconds
-- Finished in 1.8572 seconds
-- Finished in 1.7550 seconds

-- 3000000
-- Finished in 30.7137 seconds
-- Finished in 26.4237 seconds
-- Finished in 25.2911 seconds

-- 30000000
-- TODO: Need to get here!