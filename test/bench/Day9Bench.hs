module Day9Bench where

import           Criterion   (Benchmark, bench, bgroup, env, nf)
import           Data.Either (fromRight)

import           Day9

srcFile :: FilePath
srcFile = "input/day9"

input :: IO [Int]
input = getInput srcFile

tests :: [Benchmark]
tests = [
  env input $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part1" $ nf part2 x
      ]
  ]
