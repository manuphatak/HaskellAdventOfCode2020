module Day23.CircularList where

import Data.Foldable
import Data.Function
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Prelude hiding (drop, take)

newtype CircularList a = CList {getCList :: (Seq a, Seq a, Seq a)}

instance Show a => Show (CircularList a) where
  show (CList (xs, ys, _)) = show (xs >< ys)

instance Eq a => Eq (CircularList a) where
  (CList (as, bs, _)) == (CList (xs, ys, _)) = as >< bs == xs >< ys

instance Foldable CircularList where
  foldMap f (CList (xs, ys, _)) = foldMap f (xs >< ys)

fromList :: [a] -> CircularList a
fromList xs = CList (Seq.fromList xs, Empty, Empty)

toList :: CircularList a -> [a]
toList = Data.Foldable.toList

takeR :: Int -> CircularList a -> [a]
takeR n _ | n < 0 = error "n must be a positive Int"
takeR 0 _ = []
takeR n (CList (x :<| xs, ys, zs)) = x : takeR (pred n) (CList (xs, ys :|> x, zs))
takeR n (CList (Empty, ys, zs)) = takeR n (CList (ys, Empty, zs))

dropR :: Int -> CircularList a -> CircularList a
dropR n = snd . dropR' n

dropR' :: Int -> CircularList a -> (Seq a, CircularList a)
dropR' n' cList = go n' (Empty, cList)
  where
    go :: Int -> (Seq a, CircularList a) -> (Seq a, CircularList a)
    go n _ | n < 0 = error "n must be a positive Int"
    go 0 result = result
    go n (bs, CList (x :<| xs, ys, zs)) = go (pred n) (bs :|> x, CList (xs, ys, zs))
    go n (bs, CList (Empty, ys, zs)) = go n (bs, CList (ys, Empty, zs))

skipR :: Int -> CircularList a -> CircularList a
skipR n _ | n < 0 = error "n must be a positive Int"
skipR 0 xs = xs
skipR n (CList (x :<| xs, ys, zs)) = skipR (pred n) (CList (xs, ys :|> x, zs))
skipR n (CList (Empty, ys, zs)) = skipR n (CList (ys, Empty, zs))

skipL :: Int -> CircularList a -> CircularList a
skipL n _ | n < 0 = error "n must be a positive Int"
skipL 0 xs = xs
skipL n (CList (xs, ys :|> y, zs)) = skipL (pred n) (CList (y :<| xs, ys, zs))
skipL n (CList (xs, Empty, zs)) = skipL n (CList (Empty, xs, zs))

skipWhileR :: (a -> Bool) -> CircularList a -> CircularList a
skipWhileR p = when (p . peek) (skipWhileR p . skipR 1)

cons :: a -> CircularList a -> CircularList a
cons x (CList (xs, ys, zs)) = CList (x :<| xs, ys, zs)

insertMany :: Foldable t => t a -> CircularList a -> CircularList a
insertMany xs cl = foldr cons cl xs

peek :: CircularList a -> a
peek (CList (x :<| _, _, _)) = x
peek (CList (Empty, ys, zs)) = peek (CList (ys, Empty, zs))

yankR :: Int -> CircularList a -> CircularList a
yankR n = go . dropR' n
  where
    go :: (Seq a, CircularList a) -> CircularList a
    go (zs, CList (xs, ys, _)) = CList (xs, ys, zs)

putR :: CircularList a -> CircularList a
putR (CList (xs, ys, zs)) = insertMany zs (CList (xs, ys, Empty))

sortBy :: Eq a => (a -> a -> Ordering) -> CircularList a -> CircularList a
sortBy fn (CList (xs, ys, zs)) = CList (Seq.sortBy fn (a :<| as), Empty, zs) & skipWhileR (/= a)
  where
    (a :<| as) = xs >< ys

sort :: (Eq a, Ord a) => CircularList a -> CircularList a
sort = sortBy compare

when :: (p -> Bool) -> (p -> p) -> p -> p
when p fn target = if p target then fn target else target
