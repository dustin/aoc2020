module Day17Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import qualified Data.Text.IO    as TIO

import           Advent.AoC

import           Day17

tests :: [Benchmark]
tests = [
  -- env (TIO.readFile "input/day17") $ \x -> bench "parsing" $ nf (parseLit parseInput) x,
  env (getInput "input/day17") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
