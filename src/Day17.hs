module Day17 where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Advent.AoC
import           Life

type Point3 = (Int,Int,Int)

type World = Set Point3

around3 :: Point3 -> [Point3]
around3 input@(x,y,z) = [(x+xo, y+yo, z+zo)
                 | xo <- offs, yo <- offs, zo <- offs,
                   (x+xo, y+yo, z+zo) /= input]
  where offs = [-1, 0, 1]

getInput :: FilePath -> IO World
getInput fn = Set.map (\(x,y) -> (x,y,0)) . Map.keysSet . Map.filter id . parseGrid (== '#') <$> readFile fn

keepRule, newRule :: Int -> Bool
keepRule n = n == 2 || n == 3
newRule  n = n == 3
{-# INLINE keepRule #-}
{-# INLINE newRule #-}

type Point4 = (Int, Int, Int, Int)

around4 :: Point4 -> [Point4]
around4 input@(x,y,z,t) = [(x+xo, y+yo, z+zo, t+to)
                          | xo <- offs, yo <- offs, zo <- offs, to <- offs,
                            (x+xo, y+yo, z+zo, t+to) /= input]
  where offs = [-1, 0, 1]

part1 :: World -> Int
part1 = Set.size . ntimes 6 (gameOfLife around3 keepRule newRule)

part2 :: World -> Int
part2 = Set.size . ntimes 6 (gameOfLife around4 keepRule newRule) . Set.map (\(x,y,z) -> (x,y,z,0))
