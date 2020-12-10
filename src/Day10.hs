{-# LANGUAGE TupleSections #-}

module Day10 where

import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet
import           Data.List       (sort)
import qualified Data.Map.Strict as Map

import           Advent.AoC
import           Advent.Search

getInput :: FilePath -> IO [Int]
getInput = fmap (sort . fmap read . words) . readFile

part1 :: [Int] -> Int
part1 = product . Map.fromListWith (+) . fmap (,1) . diff . addEnds
  where diff xs = zipWith (-) xs (tail xs)
        addEnds xs = 0 : xs <> [maximum xs + 3]

part2 :: [Int] -> Int
part2 ins = succ . IntMap.findWithDefault 0 0 . löb . fmap f $ neighbors
  where
    f :: [Int] -> IntMap.IntMap Int -> Int
    f [] _ = 0
    f c m  = length c - 1 + sum [ IntMap.findWithDefault 0 k m | k <- c ]
    neighbors = IntMap.fromList [ (k, [n+k | n <- [1..3], IntSet.member (n+k) xss]) | k <- xs]
    xs = 0 : ins
    xss = IntSet.insert (maximum ins + 3) $ IntSet.fromList xs

--
-- Stuff below this isn't part of the Final Solution, but was interesting.
--

-- Instead of using löb, we can basically do the same thing with a map
-- which is differently magical.
part2' :: [Int] -> Int
part2' ins = maximum m
  where
    xss = IntSet.fromList (0 : ins)
    m = IntMap.fromSet ref xss
    ref i
      | i == IntSet.findMax xss = 1
      | otherwise = sum [ IntMap.findWithDefault 0 (i + j) m | j <- [1..3] ]

-- I didn't end up using this, but here's a list of all of the
-- possible arrangements.
arrangements :: [Int] -> [[Int]]
arrangements ins = filter ((== IntSet.findMax alls) . head) $ bfs neighbor [0]
  where
    alls = IntSet.fromList ins
    neighbor :: [Int] -> [[Int]]
    neighbor []       = []
    neighbor xs@(l:_) = [ (l+j):xs | j <- [1..3], (l+j) `IntSet.member` alls]
