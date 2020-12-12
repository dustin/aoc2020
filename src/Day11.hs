{-# LANGUAGE FlexibleInstances #-}
{-# Options_GHC -Wno-orphans #-}

module Day11 where

import           Data.Array.Base    (unsafeAt)
import qualified Data.Array.Unboxed as UA
import           Data.Ix            (Ix (..))
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (catMaybes)

import           Advent.AoC
import           Advent.Search
import           Advent.TwoD
import           Advent.Vis

type World = UA.UArray Point Char

getInput :: FilePath -> IO World
getInput = fmap (toArray . parseGrid id) . readFile
  where
    toArray m = UA.array (bounds2d m) (Map.assocs m)

instance Bounded2D World where
  bounds2d = UA.bounds

drawMap :: World -> IO ()
drawMap w = putStrLn . drawString w $ (w UA.!)

type WP = (World, Bool)

type Neighbors = Point -> [Int]

move :: Int -> Neighbors -> WP -> WP
move minSeat nm (w,_) = (w UA.// changes, (not.null) changes)
  where
    changes = foldMap f (UA.assocs w)
    occd :: Int -> Bool
    occd pos = w `unsafeAt` pos == '#'
    f (pos, 'L')
      | (not.any occd) (nm pos) = [(pos, '#')]
    f (pos, '#')
      | countIf occd (nm pos) >= minSeat = [(pos, 'L')]
    f _ = [] -- no change

isSeat :: Char -> Bool
isSeat '#' = True
isSeat 'L' = True
isSeat _   = False

part1 :: World -> Int
part1 w = countIf (== '#') . UA.elems . stabilize (move 4 (nm UA.!)) $ w
  where
    b = UA.bounds w
    nm :: UA.Array Point [Int]
    nm = UA.array b (fmap f $ UA.assocs w)
      where f (p, c)
              | isSeat c = (p, fmap (index b) . filter cand . aroundD $ p)
              | otherwise = (p, [])
            cand x = inRange b x && isSeat (w UA.! x)

stabilize :: (WP -> WP) -> World -> World
stabilize m w = fst . head . dropWhile snd . iterate m $ (w,True)

part2 :: World -> Int
part2 w = countIf (== '#') . UA.elems . stabilize (move 5 (nm UA.!)) $ w
  where
    b = UA.bounds w
    nm :: UA.Array Point [Int]
    nm = UA.array b (fmap f $ UA.assocs w)
      where f (p, c)
              | isSeat c = (p, index b . fst <$> findSeats p)
              | otherwise = (p, [])

    findSeats p = catMaybes (findSeat <$> aroundD (0,0))
      where
        walk (x,y) dir@(xd, yd) = (x + xd, y + yd) : walk (x + xd, y + yd) dir
        findSeat = f . walk p
          where
            f [] = Nothing
            f (p':xs)
              | not (inRange b p') = Nothing
              | isSeat (w UA.! p') = Just (p', w UA.! p')
              | otherwise          = f xs
