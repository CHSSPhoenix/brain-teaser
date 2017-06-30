module Data.USCS.TailRecursionSpec (main, spec) where

import Test.Hspec

import Data.USCS.TailRecursion

main :: IO ()
main = hspec spec

ex1, ex2, ex3, ex4, ex5 :: Tree Int
ex1 = Leaf 0
ex2 = Node (Leaf 1) (Leaf 2)
ex3 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
ex4 = Node (Leaf 3) (Node (Leaf 1) (Leaf 2))
ex5 = Node (Leaf 3) (Node (Leaf 1) (Node (Leaf 5) (Leaf 6)))
ex6 = Node (Node (Node (Leaf 1) (Leaf 3)) (Leaf 2)) (Leaf 0)

spec :: Spec
spec = mySpec splitLeftR


mySpec fTest = do
  describe "splitLeft" $ do
    it ("ex1 => " ++ show (fTest ex1)) $ do
      fTest ex1 `shouldBe` (0, Nothing)
    it ("ex2 => " ++ show (fTest ex2)) $ do
      fTest ex2 `shouldBe` (1, Just (Leaf 2))
    it ("ex3 => " ++ show (fTest ex3)) $ do
      fTest ex3 `shouldBe` (1, Just (Node (Leaf 2) (Leaf 3)))
    it ("ex4 => " ++ show (fTest ex4)) $ do
      fTest ex4 `shouldBe` (3, Just (Node (Leaf 1) (Leaf 2)))
    it ("ex5 => " ++ show (fTest ex5)) $ do
      fTest ex5 `shouldBe` (3, Just (Node (Leaf 1) (Node (Leaf 5) (Leaf 6))))
    --it ("ex6 => " ++ show (fTest ex6)) $ do
    --  fTest ex6 `shouldBe` (1, Just (Node (Node (Leaf 3) (Leaf 2)) (Leaf 0)))