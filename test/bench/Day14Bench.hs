module Day14Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import qualified Data.Text.IO    as TIO

import           Advent.AoC

import           Day14

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day14") $ \x -> bench "parsing" $ nf (parseLit parseProgram) x,
  env (getInput "input/day14") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
