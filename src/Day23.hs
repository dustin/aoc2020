module Day23 where

import qualified Data.Array.Unboxed as A
import           Data.Char          (intToDigit)
import           Data.List          (unfoldr)

import           Advent.AoC

cups :: [Int]
cups = [1,3,5,4,6,8,7,2,9]

data Game = Game {
  _pos     :: Int
  , _poses :: A.UArray Int Int
  } deriving Show

mkGame :: [Int] -> Game
mkGame l = Game (head l) m
  where m = A.array (1, maximum l) (zip l (tail (cycle l)))

walkFrom :: Int -> Game -> [Int]
walkFrom n Game{_poses} = unfoldr (\x -> Just (x, _poses A.! x)) n

play :: Game -> Game
play g@Game{..} = g{_pos = _poses' A.! _pos, _poses=_poses'}
  where
    dest = guessSeq (_pos - 1)
    next3 = take 3 (walkFrom (_poses A.! _pos) g)
    d3last = last next3
    d3next = _poses A.! last next3
    destPos = _poses A.! dest
    _poses' = _poses A.// [(_pos, d3next), (dest, head next3), (d3last, destPos)]
    guessSeq 0 = guessSeq (snd . A.bounds $ _poses)
    guessSeq n
      | elem n next3 = guessSeq (n - 1)
      | otherwise = n

playn :: Int -> Game -> Game
playn n = ntimes n play

showGame :: Game -> [Int]
showGame g@Game{..} = take 9 $ walkFrom _pos g

part1 :: Int -> [Int] -> String
part1 n = label . playn n . mkGame
  where label = fmap intToDigit . tail . take 9 . walkFrom 1

part2 :: Int
part2 = product . drop 1 . take 3 . walkFrom 1 . playn 10000000 $ g
  where g = mkGame (cups <> [10..1000000])
