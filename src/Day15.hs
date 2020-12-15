{-# LANGUAGE BangPatterns #-}

module Day15 where

import           Control.Monad               (foldM)
import           Control.Monad.ST
import           Data.Foldable               (traverse_)
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.Vector.Unboxed.Mutable as MV

import           Advent.AoC

myInput :: [Int]
myInput = [14,3,1,0,9,5]

-- Original variant
game :: [Int] -> [Int]
game inp = inp <> go (IntMap.fromList (zip inp [1..])) (length inp + 1) 0
  where
    go !m !pos !n = n : go (IntMap.insert n pos m) (pos+1) (pos - p)
      where p = IntMap.findWithDefault pos n m

-- Variation using iterate
game' :: [Int] -> [Int]
game' inp = inp <> (fmap snd3 . iterate next) start
  where start = (length inp + 1, 0, IntMap.fromList (zip inp [1..]))
        next (pos, n, m) = (succ pos, pos - p, IntMap.insert n pos m)
          where p = IntMap.findWithDefault pos n m

-- ST with mutable vectors.  Reasonable approach to get down to ~a second.
gamenst :: Int -> [Int] -> Int
gamenst nth inp = runST $ do
  prevs <- MV.replicate nth (-1)
  traverse_ (uncurry (MV.write prevs)) (zip inp [1..])
  foldM (\n pos -> do
            p <- MV.unsafeRead prevs n
            MV.unsafeWrite prevs n pos
            pure (pos - if p == -1 then pos else p)) 0 [length inp + 1 .. nth - 1]

gamen :: Int -> [Int] -> Int
gamen nth = (!! pred nth) . game

part1 :: [Int] -> Int
part1 = gamenst 2020

part2 :: [Int] -> Int
part2 = gamenst 30000000
