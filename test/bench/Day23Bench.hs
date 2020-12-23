module Day23Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day23

tests :: [Benchmark]
tests = [
  env (getInput "input/day23") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
