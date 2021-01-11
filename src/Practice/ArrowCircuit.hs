{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Practice.ArrowCircuit where

import Control.Arrow
import Control.Category (Category (..))
import qualified Control.Category as Category
import Data.Traversable (mapAccumL)
import Prelude hiding (id, (.))

-- from https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial
newtype Circuit a b = Circuit {getCircuit :: a -> (Circuit a b, b)}

instance Category Circuit where
  id = Circuit (Category.id,)
  (.) = dot
    where
      (Circuit g) `dot` (Circuit f) = Circuit $ \a ->
        let (f', b) = f a
            (g', c) = g b
         in (g' `dot` f', c)

instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a)
  first (Circuit f) = Circuit $ \(b, d) ->
    let (f', c) = f b
     in (first f', (c, d))

runCircuit :: Traversable t => Circuit a c -> t a -> t c
runCircuit cir = snd . mapAccumL getCircuit cir

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
  let (output, acc') = input `f` acc
   in (accum acc' f, output)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n
