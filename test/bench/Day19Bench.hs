module Day19Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day19

tests :: [Benchmark]
tests = [
  env (getInput "input/day19") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
