module Day13Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import qualified Data.Text.IO    as TIO

import           Advent.AoC

import           Day13

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day13") $ \x -> bench "parsing" $ nf (parseLit parseInput) x,
  env (getInput "input/day13") $ \ ~x -> bgroup "int" [
      bench "part1" $ nf part1 x
      ],
    env (getInput "input/day13") $ \ ~x -> bgroup "integer" [
      bench "part2" $ nf part2 x,
      bench "part2 (nshepperd)" $ nf part2ns x
      ]
  ]
