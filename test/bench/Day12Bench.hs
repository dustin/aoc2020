module Day12Bench where

import           Criterion          (Benchmark, bench, bgroup, env, nf)
import qualified Data.Array.Unboxed as UA
import qualified Data.Text.IO       as TIO

import           Advent.AoC

import           Day12

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day12") $ \x -> bench "parsing" $ nf (parseLit parseActions) x,
  env (getInput "input/day12") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
