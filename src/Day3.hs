module Day3 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Advent.AoC
import           Advent.TwoD
import           Advent.Vis

newtype World = World (Map Point Char) deriving (Show)

instance Bounded2D World where
  bounds2d (World m) = bounds2d m

display :: World -> String
display (World m) = drawString m (mapCharFun m id)

getInput :: FilePath -> IO World
getInput fn = parseInput <$> readFile fn

parseInput :: String -> World
parseInput = World . parseGrid id

at :: World -> (Int, Int) -> Char
at (World m) (x,y) = ans
  where ans = m Map.! (ex x, why y)
        ex x'
          | x' > maxx = ex (x' - (maxx+1))
          | otherwise = x'
        why y'
          | y' > maxy = ex (y' - (maxy+1))
          | otherwise = y'
        (_, (maxx, maxy)) = bounds2d m

slope :: Int -> (Int,Int) -> [(Int,Int)]
slope maxy (xoff, yoff) = takeWhile (\(_,y) -> y <= maxy) [(i*xoff, i*yoff) | i <- [0..]]

trees :: World -> (Int, Int) -> Int
trees w@(World m) s = length . filter (== '#') . fmap (at w) $ slope maxy s
  where
    (_, (_, maxy)) = bounds2d m

part1 :: World -> Int
part1 w = trees w (3,1)

part2 :: World -> Int
part2 w = foldr (\x -> (trees w x *)) 1 [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]
