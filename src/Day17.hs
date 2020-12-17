module Day17 where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Advent.AoC
import           Advent.Search

type Point3 = (Int,Int,Int)

type World = Set Point3

around3 :: Point3 -> [Point3]
around3 input@(x,y,z) = [(x+xo, y+yo, z+zo)
                 | xo <- offs, yo <- offs, zo <- offs,
                   (x+xo, y+yo, z+zo) /= input]
  where offs = [-1, 0, 1]

getInput :: FilePath -> IO World
getInput fn = Set.map (\(x,y) -> (x,y,0)) . Map.keysSet . Map.filter id . parseGrid (== '#') <$> readFile fn

play :: Ord k => (k -> [k]) -> Set k -> Set k
play ex w = foldMap mf $ relevant w
  where
    mf p
      | at p = let n = countp p in if n == 2 || n == 3 then Set.singleton p else mempty
      | otherwise = if countp p == 3 then Set.singleton p else mempty
    countp p = countIf id (at <$> ex p)
    at p = Set.member p w
    relevant = Set.fromList . foldMap ex

type Point4 = (Int, Int, Int, Int)

around4 :: Point4 -> [Point4]
around4 input@(x,y,z,t) = [(x+xo, y+yo, z+zo, t+to)
                          | xo <- offs, yo <- offs, zo <- offs, to <- offs,
                            (x+xo, y+yo, z+zo, t+to) /= input]
  where offs = [-1, 0, 1]

part1 :: World -> Int
part1 = Set.size . (!! 6) . iterate (play around3)

part2 :: World -> Int
part2 = Set.size . (!! 6) . iterate (play around4) . Set.map (\(x,y,z) -> (x,y,z,0))
