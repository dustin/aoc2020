{-# LANGUAGE LambdaCase #-}

module Day3 where

import           Data.Foldable   (fold)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     (Product (..), Sum (..))

import           Advent.AoC
import           Advent.TwoD

type World = Map Point (Sum Int)

getInput :: FilePath -> IO World
getInput fn = parseGrid (\case '#' -> 1; _ -> 0) <$> readFile fn

-- Count the trees along a given slope.
trees :: World -> (Int, Int) -> Int
trees m (xoff, yoff) = getSum . fold $ slope
  where
    ((maxx, maxy),_) = Map.findMax m
    pos i = (i*xoff `mod` (maxx+1), i*yoff)
    slope = [m Map.! pos i | i <- [0 .. (maxy `div` yoff)]]

part1 :: World -> Int
part1 w = trees w (3,1)

part2 :: World -> Int
part2 w = getProduct . foldMap (Product . trees w) $ [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]
