module Day10Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day10

tests :: [Benchmark]
tests = [
  env (getInput "input/day10") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2 (löb)" $ nf part2 x,
      bench "part2 (map)" $ nf part2' x,
      bench "part2 (trib)" $ nf part2trib x,
      bench "part2 (glguy)" $ nf part2gl x
      ]
  ]
