module Data.USCS.FixSpec(main, spec) where

import Test.Hspec

import Data.USCS.Fix

main :: IO ()
main = hspec spec

spec = do
  describe "myFoldrFix" $ do
    it ("myFoldrFix (+) 0 [1..5]") $ do
      myFoldrFix (+) 0 [1..5] `shouldBe` (foldr (+) 0 [1..5])
    it ("myFoldrFix (*) 1 [1..5]") $ do
          myFoldrFix (*) 1 [1..5] `shouldBe` (foldr (*) 1 [1..5])
