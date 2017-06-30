module Data.USCS.TailRecursionBench (benchmarks) where

import           Criterion
import           Data.USCS.TailRecursion


bigTree :: Int -> Tree Int
bigTree 0 = Leaf 0
bigTree n = Node (bigTree (n-1)) (Leaf 10)

tree1000 = bigTree 1000
tree200000 = bigTree 2000000

testFunction t f = fst $! f t

benchmarks :: [Benchmark]
benchmarks =
    [ bench "splitLeft Recursive 1000"          (whnf (testFunction tree1000)   splitLeft)
    , bench "splitLeft Tail Recursive 1000"     (whnf (testFunction tree1000)   splitLeftR)
    , bench "splitLeft Recursive 2000000"       (whnf (testFunction tree200000) splitLeft)
    , bench "splitLeft Tail Recursive 2000000"  (whnf (testFunction tree200000) splitLeftR)
    ]
