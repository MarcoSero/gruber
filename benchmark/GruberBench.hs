module GruberBench (benchmarks) where

import Gruber

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
