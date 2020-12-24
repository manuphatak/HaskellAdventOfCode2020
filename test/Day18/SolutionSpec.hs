module Day18.SolutionSpec (spec) where

import Data.Foldable (for_)
import Day18.Solution
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "solves Part 1" $ do
    input <- readFile "./test/Day18/input.txt"
    part1 input `shouldBe` "6811433855019"
  xit "solves Part 2" $ do
    input <- readFile "./test/Day18/input.txt"
    part2 input `shouldBe` "hello_santa"

  describe "parseExpression" $ do
    let cases =
          [ ( "1 + 2",
              3,
              BinaryOp (Number 1, Plus, Number 2)
            ),
            ( "1 + 2 * 3 + 4 * 5 + 6",
              71,
              BinaryOp
                ( BinaryOp
                    ( BinaryOp
                        ( BinaryOp
                            ( BinaryOp (Number 1, Plus, Number 2),
                              Times,
                              Number 3
                            ),
                          Plus,
                          Number 4
                        ),
                      Times,
                      Number 5
                    ),
                  Plus,
                  Number 6
                )
            ),
            ( "2 * 3 + (4 * 5)",
              26,
              BinaryOp
                ( BinaryOp (Number 2, Times, Number 3),
                  Plus,
                  BinaryOp (Number 4, Times, Number 5)
                )
            ),
            ( "5 + (8 * 3 + 9 + 3 * 4 * 3)",
              437,
              BinaryOp
                ( Number 5,
                  Plus,
                  BinaryOp
                    ( BinaryOp
                        ( BinaryOp
                            ( BinaryOp
                                ( BinaryOp (Number 8, Times, Number 3),
                                  Plus,
                                  Number 9
                                ),
                              Plus,
                              Number 3
                            ),
                          Times,
                          Number 4
                        ),
                      Times,
                      Number 3
                    )
                )
            ),
            ( "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
              12240,
              BinaryOp
                ( BinaryOp (Number 5, Times, Number 9),
                  Times,
                  BinaryOp
                    ( BinaryOp
                        ( BinaryOp
                            ( BinaryOp
                                ( BinaryOp (Number 7, Times, Number 3),
                                  Times,
                                  Number 3
                                ),
                              Plus,
                              Number 9
                            ),
                          Times,
                          Number 3
                        ),
                      Plus,
                      BinaryOp
                        ( BinaryOp (Number 8, Plus, Number 6),
                          Times,
                          Number 4
                        )
                    )
                )
            ),
            ( "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",
              13632,
              BinaryOp
                ( BinaryOp
                    ( BinaryOp
                        ( BinaryOp
                            ( BinaryOp
                                ( BinaryOp
                                    ( BinaryOp (Number 2, Plus, Number 4),
                                      Times,
                                      Number 9
                                    ),
                                  Times,
                                  BinaryOp
                                    ( BinaryOp
                                        ( BinaryOp (Number 6, Plus, Number 9),
                                          Times,
                                          Number 8
                                        ),
                                      Plus,
                                      Number 6
                                    )
                                ),
                              Plus,
                              Number 6
                            ),
                          Plus,
                          Number 2
                        ),
                      Plus,
                      Number 4
                    ),
                  Times,
                  Number 2
                )
            )
          ]
    let test (input, evaluated, parsed) = context ("given an expression of " ++ show input) $ do
          it "can be parsed" $ do
            parseExpression input `shouldBe` Right parsed
          it ("is evaluates to " ++ show evaluated) $ do
            evaluateExpression parsed `shouldBe` evaluated
    for_ cases test
