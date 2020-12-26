{-# LANGUAGE TemplateHaskellQuotes #-}

module Day18.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day18.Internal (drawExpression)
import Day18.Solution
  ( advancedTable,
    basicTable,
    evaluateExpression,
    parseExpression,
    part1,
    part2,
  )
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day18/input.txt"
    part1 input `shouldBe` "6811433855019"
  it "solves Part 2" $ do
    input <- readFile "./test/Day18/input.txt"
    part2 input `shouldBe` "129770152447927"

  describe "parseExpression" $ do
    let test prefix table (i, (input, expected)) = context ("given the expression " ++ show input) $ do
          it "can be parsed" $ do
            let filename = show 'parseExpression ++ "_example_" ++ prefix ++ "_" ++ show i
            let Right output = parseExpression table input
            defaultGolden filename (drawExpression output)

          it ("evaluates to " ++ show expected) $ do
            (fmap evaluateExpression . parseExpression table) input `shouldBe` Right expected

    context "given the basic math table " $ do
      let cases =
            [ ("1 + 2", 3 :: Int),
              ("1 + 2 * 3 + 4 * 5 + 6", 71),
              ("1 + (2 * 3) + (4 * (5 + 6))", 51),
              ("2 * 3 + (4 * 5)", 26),
              ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
              ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
              ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
            ]

      for_ (zip [0 :: Int ..] cases) (test "basic" basicTable)
    context "given the advanced math table " $ do
      let cases =
            [ ("1 + 2", 3 :: Int),
              ("1 + 2 * 3 + 4 * 5 + 6", 231),
              ("1 + (2 * 3) + (4 * (5 + 6))", 51),
              ("2 * 3 + (4 * 5)", 46),
              ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
              ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
              ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340)
            ]

      for_ (zip [0 :: Int ..] cases) (test "advanced" advancedTable)
