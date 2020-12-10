module Day8Bench where

import           Criterion   (Benchmark, bench, bgroup, env, nf)
import           Data.Either (fromRight)

import           Day8

tests :: [Benchmark]
tests = [
  env (getInput "input/day8") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
