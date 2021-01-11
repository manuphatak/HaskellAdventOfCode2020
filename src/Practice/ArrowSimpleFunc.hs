{-# LANGUAGE Arrows #-}

module Practice.ArrowSimpleFunc where

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

-- https://wiki.haskell.org/Arrow_tutorial
newtype SimpleFunc a b = SimpleFunc {runF :: a -> b}

instance Category SimpleFunc where
  id = arr id
  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)

instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first (SimpleFunc f) = SimpleFunc (mapFst f)
    where
      mapFst g (b, d) = (g b, d)
  second (SimpleFunc f) = SimpleFunc (mapSnd f)
    where
      mapSnd g (d, b) = (d, g b)

unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry

liftA2 :: Arrow cat => (c1 -> c2 -> c3) -> cat a c1 -> cat a c2 -> cat a c3
liftA2 op f g = f &&& g >>> unsplit op

f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> x * 3 + 1)

h :: SimpleFunc Int Int
h = liftA2 (+) f g

h' :: SimpleFunc Int Int
h' = proc x -> do
  fx <- f -< x
  gx <- g -< x

  returnA -< (fx + gx)
