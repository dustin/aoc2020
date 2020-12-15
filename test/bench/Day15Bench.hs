module Day15Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day15

tests :: [Benchmark]
tests = [
  env (pure myInput) $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
