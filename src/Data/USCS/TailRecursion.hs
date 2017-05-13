module Data.USCS.TailRecursion (Tree (..), splitLeft) where

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show, Eq)

splitLeft :: Tree a -> (a, Maybe (Tree a))
splitLeft (Leaf a)   = (a, Nothing)
splitLeft (Node l r) = case splitLeft l of
                            (a,Nothing)->(a,Just r)
                            (a,Just l')->(a,Just (Node l' r))

