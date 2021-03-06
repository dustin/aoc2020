module Day4Bench where

import           Criterion    (Benchmark, bench, bgroup, env, nf)
import qualified Data.Text.IO as TIO

import           Advent.AoC
import           Day4

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day4") $ \x -> bench "parsing" $ nf (parseLit parsePassport) x,
  env (getInput "input/day4") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
