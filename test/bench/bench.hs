module Main (main) where

import           Criterion      (bgroup)
import           Criterion.Main (defaultMain)

import qualified Day8Bench
import qualified Day9Bench

main :: IO ()
main = defaultMain [
  bgroup "day8" Day8Bench.tests,
  bgroup "day9" Day9Bench.tests
  ]
