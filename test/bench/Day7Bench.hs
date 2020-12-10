module Day7Bench where

import           Criterion   (Benchmark, bench, bgroup, env, nf)
import           Data.Either (fromRight)

import           Day7

tests :: [Benchmark]
tests = [
  env (getInput "input/day7") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x,
      bench "part2 (löb)" $ nf part2' x
      ]
  ]
