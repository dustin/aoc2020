module Day17 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Advent.AoC
import           Advent.Search

type Point3 = (Int,Int,Int)

type World = Map Point3 Bool

around3 :: Point3 -> [Point3]
around3 input@(x,y,z) = [(x+xo, y+yo, z+zo)
                 | xo <- offs, yo <- offs, zo <- offs,
                   (x+xo, y+yo, z+zo) /= input]
  where offs = [-1, 0, 1]

getInput :: FilePath -> IO World
getInput fn = Map.mapKeys (\(x,y) -> (x,y,0)) . parseGrid (== '#') <$> readFile fn

play :: Ord k => (k -> [k]) -> Map k Bool -> Map k Bool
play ex w = Map.fromList . fmap mf $ relevant w
  where
    mf p = modf (p, at p)
    modf (p, False) = (\n -> (p, n == 3)) $ countIf id (at <$> ex p)
    modf (p, True)  = (\n -> (p, n == 2 || n == 3)) $ countIf id (at <$> ex p)
    at p = Map.findWithDefault False p w

    relevant = Set.toList . Set.fromList . foldMap ex . Map.keys . Map.filter id

type Point4 = (Int, Int, Int, Int)

around4 :: Point4 -> [Point4]
around4 input@(x,y,z,t) = [(x+xo, y+yo, z+zo, t+to)
                          | xo <- offs, yo <- offs, zo <- offs, to <- offs,
                            (x+xo, y+yo, z+zo, t+to) /= input]
  where offs = [-1, 0, 1]

part1 :: World -> Int
part1 = countIf id . Map.elems . (!! 6) . iterate (play around3)

part2 :: World -> Int
part2 = countIf id . Map.elems . (!! 6) . iterate (play around4) . Map.mapKeys (\(x,y,z) -> (x,y,z,0))
