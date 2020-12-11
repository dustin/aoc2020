{-# LANGUAGE FlexibleInstances #-}
{-# Options_GHC -Wno-orphans #-}

module Day11 where

import qualified Data.Array         as A
import qualified Data.Array.Unboxed as UA
import           Data.Ix            (Ix (..))
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (catMaybes)

import           Advent.AoC
import           Advent.Search
import           Advent.TwoD
import           Advent.Vis

type World = UA.Array Point Char

data Position = Floor | Empty | Occupied

getInput :: FilePath -> IO World
getInput = fmap (toArray . parseGrid id) . readFile
  where
    toArray m = UA.array (bounds2d m) (Map.assocs m)

instance Bounded2D (UA.Array Point Char) where
  bounds2d = UA.bounds

drawMap :: World -> IO ()
drawMap w = putStrLn . drawString w $ (w UA.!)

part1 :: World -> Int
part1 w = countIf (== '#') . UA.elems $ stable
  where (Just stable) = findRepeatedOn (UA.assocs) . iterate move $ w
        move :: UA.Array Point Char -> UA.Array Point Char
        move w' = UA.array (UA.bounds w') (fmap f (UA.assocs w'))
          where
            f (pos, 'L')
              | any occupied (aroundD pos) = (pos, 'L')
              | otherwise = (pos, '#')
            f (pos, '#')
              | countIf occupied (aroundD pos) >= 4 = (pos, 'L')
            f x = x

            occupied p'
              | inRange (UA.bounds w) p' = w' UA.! p' == '#'
              | otherwise = False

findSeats :: World -> Point -> [(Point, Char)]
findSeats w p = catMaybes (findSeat <$> aroundD (0,0))
  where
    walk (x,y) dir@(xd, yd) = (x + xd, y + yd) : walk (x + xd, y + yd) dir
    findSeat = f . walk p
      where
        f [] = Nothing
        f (p':xs)
          | not (inRange (UA.bounds w) p') = Nothing
          | isSeat p' = Just (p', w UA.! p')
          | otherwise = f xs
        isSeat p' = (w UA.! p') `elem` ['L', '#']

part2 :: World -> Int
part2 w = countIf (== '#') . UA.elems $ stable
  where
    (Just stable) = findRepeatedOn (UA.assocs) . iterate (move nMap) $ w
    nMap :: A.Array Point [Point]
    nMap = A.array (A.bounds w) (fmap f $ UA.assocs w)
      where f (p, '#') = (p, fst <$> findSeats w p)
            f (p, 'L') = (p, fst <$> findSeats w p)
            f (p, _)   = (p, [])

    move :: A.Array Point [Point] -> UA.Array Point Char -> UA.Array Point Char
    move nm w' = UA.array (UA.bounds w') (fmap f (UA.assocs w'))
      where
        occd pos = w' UA.! pos == '#'
        f :: (Point, Char) -> (Point, Char)
        f (pos, 'L')
          | any occd (nm A.! pos) = (pos, 'L')
          | otherwise = (pos, '#')
        f (pos, '#')
          | countIf occd (nm A.! pos) >= 5 = (pos, 'L')
        f (pos, x) = (pos, x)
