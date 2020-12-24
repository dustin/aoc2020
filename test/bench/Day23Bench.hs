module Day23Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf)

import           Day23

cupsEx :: [Int]
cupsEx = [3,8,9,1,2,5,4,6,7]

tests :: [Benchmark]
tests = [
  bench "part1 array" $ nf (take 9 . walkFrom 1 . play) (mkGame cupsEx),
  bench "part1 array big" $ nf (take 9 . walkFrom 1 . play) (mkGame (cupsEx <> [10..1000000])),
  bench "part1 (100 ex ST)" $ nf (part1 100) cupsEx,
  bench "part2 (2,000,000 ex)" $ nf (part2 2000000) cupsEx
  ]
