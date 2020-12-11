module Day11 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)

import           Advent.AoC
import           Advent.Search
import           Advent.TwoD
import           Advent.Vis

type World = Map Point Char

data Position = Floor | Empty | Occupied

getInput :: FilePath -> IO World
getInput = fmap (parseGrid id) . readFile

occupied :: World -> Point -> Bool
occupied w p = Map.findWithDefault '.' p w == '#'

drawMap :: World -> IO ()
drawMap w = putStrLn . drawString w $ (w Map.!)

part1 :: World -> Int
part1 w = length . Map.toList . Map.filter (== '#') $ stable
  where (Just stable) = findRepeated . iterate move $ w
        move w' = Map.mapWithKey f w'
          where
            f pos 'L'
              | any (occupied w') (aroundD pos) = 'L'
              | otherwise = '#'
            f pos '#'
              | countIf (occupied w') (aroundD pos) >= 4 = 'L'
            f _ x = x

findSeats :: World -> Point -> [(Point, Char)]
findSeats w p = catMaybes (findSeat <$> aroundD (0,0))
  where
    ((minx, miny), (maxx, maxy)) = bounds2d w
    walk (x,y) dir@(xd, yd) = (x + xd, y + yd) : walk (x + xd, y + yd) dir
    oob (x,y) = x < minx || y < miny || x > maxx || y > maxy
    findSeat = f . walk p
      where
        f [] = Nothing
        f (p':xs)
          | oob p' = Nothing
          | isSeat p' = Just (p', Map.findWithDefault '.' p' w)
          | otherwise = f xs
        isSeat p' = (Map.findWithDefault '.' p' w) `elem` ['L', '#']

part2 :: World -> Int
part2 w = length . Map.toList . Map.filter (== '#') $ stable
  where
    (Just stable) = findRepeated . iterate (move nMap) $ w
    nMap = Map.mapWithKey f w
      where f p '#' = fst <$> findSeats w p
            f p 'L' = fst <$> findSeats w p
            f _ _   = []

    move nm w' = Map.mapWithKey f w'
      where
        occd pos = Map.findWithDefault '.' pos w' == '#'
        f pos 'L'
          | any occd (Map.findWithDefault [] pos nm) = 'L'
          | otherwise = '#'
        f pos '#'
          | countIf occd (Map.findWithDefault [] pos nm) >= 5 = 'L'
        f _ x = x
