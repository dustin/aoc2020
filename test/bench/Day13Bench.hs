module Day13Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day13

tests :: [Benchmark]
tests = [
  env (getInput "input/day13") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x,
      bench "part2 (nshepperd)" $ nf part2ns x
      ]
  ]
