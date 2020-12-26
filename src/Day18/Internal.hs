module Day18.Internal (drawExpression) where

import Day18.Solution (Expression (..))

-- inspired by the draw functions in the `containers` package
-- https://hackage.haskell.org/package/containers-0.6.4.1/docs/src/Data.Tree.html#drawTree
drawExpression :: Expression -> String
drawExpression = unlines . draw

draw :: Expression -> [String]
draw n@(Number _) = [show n]
draw (BinaryOp (a, b, c)) =
  "BinaryOp" :
  shift "  ( " "    " (draw a)
    ++ ["  , " ++ show b]
    ++ shift "  , " "    " (draw c)
    ++ ["  )"]
  where
    shift first other = zipWith (++) (first : repeat other)
