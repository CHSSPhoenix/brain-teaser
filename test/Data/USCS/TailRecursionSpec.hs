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

spec :: Spec
spec = do
  describe "splitLeft" $ do
    it ("ex1 => " ++ show ex1) $ do
      splitLeft ex1 `shouldBe` (0, Nothing)
    it ("ex2 => " ++ show ex2) $ do
      splitLeft ex2 `shouldBe` (1, Just (Leaf 2))
    it ("ex3 => " ++ show ex3) $ do
      splitLeft ex3 `shouldBe` (1, Just (Node (Leaf 2) (Leaf 3)))
    it ("ex4 => " ++ show ex4) $ do
      splitLeft ex4 `shouldBe` (3, Just (Node (Leaf 1) (Leaf 2)))
    it ("ex5 => " ++ show ex5) $ do
      splitLeft ex5 `shouldBe` (3, Just (Node (Leaf 1) (Node (Leaf 5) (Leaf 6))))