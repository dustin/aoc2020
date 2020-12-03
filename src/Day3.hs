module Day3 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Advent.AoC
import           Advent.TwoD
import           Advent.Vis

type World = Map Point Char

getInput :: FilePath -> IO World
getInput fn = parseGrid id <$> readFile fn

-- Count the trees along a given slope.
trees :: World -> (Int, Int) -> Int
trees m (xoff,yoff) = length . filter (== '#') . fmap at $ slope
  where
    (_, (maxx, maxy)) = bounds2d m
    at (x,y) = m Map.! (x `mod` (maxx+1), y `mod` (maxy+1))
    slope = takeWhile ((<= maxy) . snd) [(i*xoff, i*yoff) | i <- [0..]]

part1 :: World -> Int
part1 w = trees w (3,1)

part2 :: World -> Int
part2 w = product . fmap (trees w) $ [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]
