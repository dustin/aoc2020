module Main (main) where

import           Criterion      (bgroup)
import           Criterion.Main (defaultMain)

import qualified Day10Bench
import qualified Day11Bench
import qualified Day12Bench
import qualified Day13Bench
import qualified Day14Bench
import qualified Day1Bench
import qualified Day2Bench
import qualified Day3Bench
import qualified Day4Bench
import qualified Day5Bench
import qualified Day6Bench
import qualified Day7Bench
import qualified Day8Bench
import qualified Day9Bench

main :: IO ()
main = defaultMain [
  bgroup "day1" Day1Bench.tests,
  bgroup "day2" Day2Bench.tests,
  bgroup "day3" Day3Bench.tests,
  bgroup "day4" Day4Bench.tests,
  bgroup "day5" Day5Bench.tests,
  bgroup "day6" Day6Bench.tests,
  bgroup "day7" Day7Bench.tests,
  bgroup "day8" Day8Bench.tests,
  bgroup "day9" Day9Bench.tests,
  bgroup "day10" Day10Bench.tests,
  bgroup "day11" Day11Bench.tests,
  bgroup "day12" Day12Bench.tests,
  bgroup "day13" Day13Bench.tests,
  bgroup "day14" Day14Bench.tests
  ]
