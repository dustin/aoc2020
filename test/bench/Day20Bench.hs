module Day20Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day20

tests :: [Benchmark]
tests = [
  env (getInput "input/day20") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
