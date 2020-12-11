{-# LANGUAGE FlexibleInstances #-}
{-# Options_GHC -Wno-orphans #-}

module Day11 where

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

instance Bounded2D (UA.UArray Point Char) where
  bounds2d = UA.bounds

drawMap :: World -> IO ()
drawMap w = putStrLn . drawString w $ (w UA.!)

type WP = (World, Bool)

move :: Int -> UA.Array Point [Point] -> WP -> WP
move minSeat nm (w,_) = (w UA.// changes, (not.null) changes)
  where
    changes = foldMap f (UA.assocs w)
    occd pos = w UA.! pos == '#'
    f (pos, 'L')
      | any occd (nm UA.! pos) = [] -- no change
      | otherwise = [(pos, '#')]
    f (pos, '#')
      | countIf occd (nm UA.! pos) >= minSeat = [(pos, 'L')]
    f _ = [] -- no change

isSeat :: Char -> Bool
isSeat '#' = True
isSeat 'L' = True
isSeat _   = False

part1 :: World -> Int
part1 w = countIf (== '#') . UA.elems . stabilize (move 4 nm) $ w
  where
    nm = UA.array (UA.bounds w) (fmap f $ UA.assocs w)
      where f (p, c)
              | isSeat c = (p, filter inBounds . aroundD $ p)
              | otherwise = (p, [])
            inBounds = inRange (UA.bounds w)

stabilize :: (WP -> WP) -> World -> World
stabilize m w = fst . head . dropWhile snd . iterate m $ (w,True)

part2 :: World -> Int
part2 w = countIf (== '#') . UA.elems . stabilize (move 5 nMap) $ w
  where
    nMap = UA.array (UA.bounds w) (fmap f $ UA.assocs w)
      where f (p, c)
              | isSeat c = (p, fst <$> findSeats p)
              | otherwise = (p, [])

    findSeats p = catMaybes (findSeat <$> aroundD (0,0))
      where
        walk (x,y) dir@(xd, yd) = (x + xd, y + yd) : walk (x + xd, y + yd) dir
        findSeat = f . walk p
          where
            f [] = Nothing
            f (p':xs)
              | not (inRange (UA.bounds w) p') = Nothing
              | isSeat (w UA.! p')             = Just (p', w UA.! p')
              | otherwise                      = f xs
