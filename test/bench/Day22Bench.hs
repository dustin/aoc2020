module Day22Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day22

tests :: [Benchmark]
tests = [
  env (getInput "input/day22") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  -- This is particularly interesting input.
  , env (getInput "input/day22.sim642") $ \ ~x -> bench "sim642" $ nf part2 x
  ]