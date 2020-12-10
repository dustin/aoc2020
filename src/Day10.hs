{-# LANGUAGE TupleSections #-}

module Day10 where

import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet
import           Data.List       (group, sort)
import qualified Data.Map.Strict as Map
import           Data.Semigroup  (Product (..))
import qualified Data.Vector     as V

import           Advent.AoC
import           Advent.Search

getInput :: FilePath -> IO [Int]
getInput = fmap (sort . fmap read . words) . readFile

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

part1 :: [Int] -> Int
part1 = product . Map.fromListWith (+) . fmap (,1) . diff . addEnds
  where addEnds xs = 0 : xs <> [maximum xs + 3]

part2 :: [Int] -> Int
part2 ins = succ . maximum . löb . fmap f $ neighbors
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

-- Someone pointed out that the length of 1-gaps corresponds to a
-- tribonacci sequence and we can use that as a multiplier.  I didn't
-- see that particular pattern because my head isn't very mathy, but
-- it's a lot better than brute force.
--
-- As explained to me (by jle), if all numbers were present, the path
-- from any position would be:
--
-- > paths(i) = paths(i+1) + paths(i+2) + paths(i+3)
--
-- ...but since we have gaps of exactly three or one, then we only get
-- those sometimes, so we have a mix of tribonacci expansions and
-- boring single-path NOOPs.  So we multiply the tribonaccis we find,
-- considering everything else a NOOP and we get the answer.
part2trib :: [Int] -> Int
part2trib = getProduct . foldMap lengths . group . diff . addEnds
  where
    lengths xs@(1:_) = Product (trib (length xs))
    lengths _        = 1
    addEnds xs = 0 : xs
    trib = (tribs V.!)
      where tribs = V.fromList [1, 1, 2, 4, 7, 13, 24]

-- I didn't end up using this, but here's a list of all of the
-- possible arrangements (from the end, `fmap reverse` if you want
-- them from the beginning.
arrangements :: [Int] -> [[Int]]
arrangements ins = filter ((== IntSet.findMax alls) . head) $ bfs neighbor [0]
  where
    alls = IntSet.fromList ins
    neighbor :: [Int] -> [[Int]]
    neighbor []       = []
    neighbor xs@(l:_) = [ (l+j):xs | j <- [1..3], (l+j) `IntSet.member` alls]
