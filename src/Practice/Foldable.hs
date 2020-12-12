module Practice.Foldable where

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Node x treeA treeB) =
    foldMap f treeA
      `mappend` f x
      `mappend` foldMap f treeB

testTree :: Tree Int
testTree =
  Node
    5
    ( Node
        3
        (Node 1 Empty Empty)
        (Node 6 Empty Empty)
    )
    ( Node
        9
        (Node 8 Empty Empty)
        (Node 10 Empty Empty)
    )
