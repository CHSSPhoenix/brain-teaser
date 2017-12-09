module Data.USCS.Fix where

fix :: (a -> a) -> a
fix f = f (fix f)

-- taken from https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
fact :: Int -> Int
fact n = if n == 0
         then 1
         else n * fact (n-1)

factFix = fix (\rec n -> if n == 0
                         then 1
                         else n * rec (n-1))

-- taken from the definition of GHC
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b = go
  where go []     = b
        go (a:as) = a `f` (go as)

-- our definition
myFoldrFix :: (a -> b -> b) -> b -> [a] -> b
myFoldrFix = fix (\rec f b l -> if null l
                                then b
                                else f (head l) (rec f b (tail l)))