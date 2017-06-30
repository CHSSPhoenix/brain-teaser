module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified Data.USCS.TailRecursionBench as TR

main :: IO ()
main = defaultMain
    [ bgroup "splitLeft" TR.benchmarks
    ]