module Day08.Utils where

import qualified Data.IntMap.Strict as IntMap

asIntMap :: [a] -> IntMap.IntMap a
asIntMap = IntMap.fromList . zip [0 ..]

-- http://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Combinators.html
-- as `fromLeft'`
fromLeftOrError :: Either a b -> a
fromLeftOrError (Right _) = error "fromLeftOrError: Argument takes form 'Right _'"
fromLeftOrError (Left x) = x

-- http://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Combinators.html
-- as `fromRight'`
fromRightOrError :: Either a b -> b
fromRightOrError (Left _) = error "fromRightOrError: Argument takes form 'Left _'"
fromRightOrError (Right x) = x

fromRightOrError' :: Show a => Either a b -> b
fromRightOrError' (Left x) = error (show x)
fromRightOrError' (Right x) = x
