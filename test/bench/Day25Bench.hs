module Day25Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day25

tests :: [Benchmark]
tests = [
  bench "part1" $ nf (part1 3418282) 8719412
  ]
