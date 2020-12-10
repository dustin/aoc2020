module Day1Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day1

tests :: [Benchmark]
tests = [
  env getInput $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
