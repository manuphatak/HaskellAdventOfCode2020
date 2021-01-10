module Day16.Utils where

import qualified Data.IntMap.Strict as IntMap

toIntMap :: [a] -> IntMap.IntMap a
toIntMap = IntMap.fromList . zip [0 ..]
