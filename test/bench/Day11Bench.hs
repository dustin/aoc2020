module Day11Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day11

tests :: [Benchmark]
tests = [
  env (getInput "input/day11") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
