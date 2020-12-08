module Main (main) where

import           Criterion      (bgroup)
import           Criterion.Main (defaultMain)

import           Day8Bench

main :: IO ()
main = defaultMain [
  bgroup "day8" Day8Bench.tests
  ]
