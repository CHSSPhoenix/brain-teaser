module Data.USCS.TailRecursion (Tree (..), splitLeft, splitTR, splitLeftP, splitLeftQ, splitLeftR) where

import           Data.Maybe

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show, Eq)

-- exercise implementation
splitLeft :: Tree a -> (a, Maybe (Tree a))
splitLeft (Leaf a)   = (a, Nothing)
splitLeft (Node l r) = case splitLeft l of
                            (a,Nothing)->(a,Just r)
                            (a,Just l')->(a,Just (Node l' r))

-- first attempt: building the tree programmatically
splitTR :: Tree a -> Maybe (Tree a)
splitTR t = splitTR' t Nothing
  where splitTR' (Leaf a) mt   = mt
        splitTR' (Node l r) mt = combine (splitTR' l mt) r

        combine :: Maybe (Tree a) -> Tree a -> Maybe (Tree a)
        combine Nothing t  = Just t
        combine (Just l) r = Just (Node l r)

-- reimplementing splitLeft with a combinator function
splitLeftP :: Tree a -> (a, Maybe (Tree a))
splitLeftP (Leaf a)   = (a, Nothing)
splitLeftP (Node l r) = (splitLeftP l) `combine` r
  where combine (s, Nothing) t  = (s, Just t)
        combine (s, Just t1) t2 = (s, Just (Node t1 t2))

-- using an accumulator for tail recursive
splitLeftQ (Leaf a)    acc = acc a
splitLeftQ (Node l r2) acc = splitLeftQ l (\min -> let (val, r1) = acc min
                                                   in (min, r1 `combine` r2))
  where combine Nothing t    = Just t
        combine (Just t1) t2 = Just (Node t2 t1)

-- splitLeft tail recursive version Root call
splitLeftR :: Tree a -> (a, Maybe (Tree a))
splitLeftR t = splitLeftQ t (\min -> (min, Nothing))

{-
-- trying to refactor splitLeftQ
splitLeftS (Leaf a)    acc = acc a
splitLeftS (Node l r2) acc = splitLeftS l (\min -> let (val, r) = acc min
                                                   in (min, maybe (Just r2) (\r1 -> Just (Node r2 r1)) r))
-}
