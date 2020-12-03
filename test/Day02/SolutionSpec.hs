module Day02.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day02.Solution (lineIsValidV1, lineIsValidV2, part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  it "solves Part 1" $ do
    input <- readFile "./test/Day02/input.txt"
    part1 input `shouldBe` "378"
  it "solves Part 2" $ do
    input <- readFile "./test/Day02/input.txt"
    part2 input `shouldBe` "280"

  let cases =
        [ ("1-3 a: abcde", Just True, Just True),
          ("1-3 b: cdefg", Just False, Just False),
          ("2-9 c: ccccccccc", Just True, Just False)
        ]
      testV1 (line, expectedV1, _) = do
        context ("given a line " ++ line) $ do
          it ("validates as " ++ show expectedV1) $ lineIsValidV1 line `shouldBe` expectedV1
      testV2 (line, _, expectedV2) = do
        context ("given a line " ++ line) $ do
          it ("validates as " ++ show expectedV2) $ lineIsValidV2 line `shouldBe` expectedV2
   in do
        describe "lineIsValidV1" $ for_ cases testV1
        describe "lineIsValidV2" $ for_ cases testV2