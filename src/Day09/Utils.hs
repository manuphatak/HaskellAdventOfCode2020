module Day09.Utils where

rollingChunks :: Int -> [a] -> [[a]]
rollingChunks size xs =
  [ take size . drop i $ xs
    | i <- [0 .. (length xs - size)]
  ]
