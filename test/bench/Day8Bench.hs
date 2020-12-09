module Day8Bench where

import           Criterion   (Benchmark, bench, bgroup, env, nf)
import           Data.Either (fromRight)

import           Computer
import           Day8

srcFile :: FilePath
srcFile = "input/day8"

input :: IO Program
input = getInput srcFile

tests :: [Benchmark]
tests = [
  env input $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
