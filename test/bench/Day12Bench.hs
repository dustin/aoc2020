module Day12Bench where

import           Criterion          (Benchmark, bench, bgroup, env, nf)
import qualified Data.Array.Unboxed as UA
import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO

import           Advent.AoC

import           Day12

parseTest :: Text -> [Action]
parseTest = parseLit parseActions

tests :: [Benchmark]
tests = [
  env (TIO.readFile "input/day12") $ \x -> bench "parsing" $ nf parseTest x,
  env (getInput "input/day12") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
