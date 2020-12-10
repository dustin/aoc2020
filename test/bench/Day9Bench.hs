module Day9Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day9

tests :: [Benchmark]
tests = [
  env (getInput "input/day9") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf (part2' 1539) x
      ]
  ]
