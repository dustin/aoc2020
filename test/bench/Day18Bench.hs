module Day18Bench where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import qualified Data.Text.IO    as TIO

import           Advent.AoC

import           Day18

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day18") $ \x -> bgroup "parsing" [
      bench "flat"$  nf (parseLit (parseExprs flat)) x,
      bench "plusFirst" $ nf (parseLit (parseExprs plusFirst)) x
      ],
  env (getInput flat "input/day18") $ \ ~x -> bgroup "flat" [
      bench "part1" $ nf part1 x
      ],
  env (getInput plusFirst "input/day18") $ \ ~x -> bgroup "plusFirst" [
      bench "part2" $ nf part2 x
      ]
  ]
