module Day24Bench where

import           Control.DeepSeq    (NFData (..), rwhnf)
import           Criterion          (Benchmark, bench, bgroup, env, nf)
import qualified Data.Array.Unboxed as UA

import           Day24

instance (NFData i, NFData e) => NFData (UA.UArray i e) where rnf = rwhnf

tests :: [Benchmark]
tests = [
  env (getInput "input/day24") $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part2" $ nf part2 x
      ]
  ]
