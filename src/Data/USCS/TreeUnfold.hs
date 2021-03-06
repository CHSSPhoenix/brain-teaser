module Data.USCS.TreeUnfold where

{-
Exercise 2.2.2 (Tree unfold).

Recall unfoldr:

  unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
  unfoldr next x =
    case next x of
      Nothing ->[]
      Just (y,r)->y:unfoldr next r

We can define an unfold function for trees as well:

  data Tree a = Leaf a
              | Node (Tree a) (Tree a)
  deriving Show

  unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a unfoldTree next x =
    case next x of
      Left y ->Leaf y
      Right (l,r)->Node (unfoldTree next l) (unfoldTree next r)

Task. Define the following functions using unfoldr or unfoldTree:

    iterate :: (a->a)->a->[a]



(The call iterate f x generates the infinite list [x, f x, f (f x), ...].)
    map :: (a->b)->[a]->[b]
        (As defined in the prelude.)

    balanced :: Int -> Tree ()

(Generates a balanced tree of the given height.)
    sized :: Int -> Tree Int

(Generates any tree with the given number of nodes. Each leaf should have a unique label.)

-}

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next x = case next x of
                    Nothing     -> []
                    Just (y, r) -> y : unfoldr next r

--next :: s -> Maybe (a, s)
next s = if s > 0
         then Just (s, s-1)
         else Nothing

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a
unfoldTree next x
  = case next x of
      Left y       -> Leaf y
      Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r)


--Task. Define the following functions using unfoldr or unfoldTree:

myIterate :: (a -> a) -> a -> [a]
--myIterate f = unfoldr (\s -> let ss = f s in Just (s, ss))
myIterate f = unfoldr (\s -> Just . ((,) s) $ f s)

myIterateTree :: (a -> a) -> a -> Tree a
myIterateTree f = unfoldTree (\s -> let ss = f s
                                    in Right (ss, s))

myMap :: (a -> b) -> [a] -> [b]
myMap f list = unfoldr (\s -> if null s
                              then Nothing
                              else Just (f (head s), tail s)) list

balanced :: Int -> Tree ()
balanced = unfoldTree (\s -> if s == 1
                             then Left ()
                             else Right (s-1, s-1))

-- Generates any tree with the given number of nodes. Each leaf should have a unique label.
sized :: Int -> Tree Int
sized n = unfoldTree (\(s,num) -> case s of
                                    0 -> Left num
                                    _ -> let s' = s - 1
                                             a = s' `div` 2
                                             b = s' `mod` 2
                                          in Right ((a + b, (num + s) * 2), (a,num + 1))) (n,1)