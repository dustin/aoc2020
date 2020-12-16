module Day16Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import qualified Data.Text.IO    as TIO

import           Advent.AoC

import           Day16

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day16") $ \x -> bench "parsing" $ nf (parseLit parseInput) x,
  env (getInput "input/day16") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
