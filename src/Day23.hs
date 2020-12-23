module Day23 where

import           Data.Char          (intToDigit)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import           Data.List          (unfoldr)

import           Advent.AoC

cups :: [Int]
cups = [1,3,5,4,6,8,7,2,9]

data Game = Game {
  _maxn    :: Int
  , _pos   :: Int
  , _poses :: IntMap Int
  } deriving Show

mkGame :: [Int] -> Game
mkGame l = Game (fst $ Map.findMax m) (head l) m
  where m = Map.fromList (zip l (tail (cycle l)))

walkFrom :: Game -> Int -> [Int]
walkFrom Game{_poses} = unfoldr (\x -> Just (x, _poses Map.! x))

play :: Game -> Game
play g@Game{..} = g{_pos = _poses' Map.! _pos, _poses=_poses'}
  where
    dest = guessSeq (_pos - 1)
    next3 = take 3 (walkFrom g (_poses Map.! _pos))
    d3last = last next3
    d3next = _poses Map.! last next3
    destPos = _poses Map.! dest
    _poses' = Map.fromList [(_pos, d3next), (dest, head next3), (d3last, destPos)] <> _poses
    guessSeq 0 = guessSeq _maxn
    guessSeq n
      | elem n next3 = guessSeq (n - 1)
      | otherwise = n

playn :: Int -> Game -> Game
playn n = ntimes n play

showGame :: Game -> [Int]
showGame g@Game{..} = take 9 $ walkFrom g _pos

part1 :: Int -> [Int] -> String
part1 n = label . playn n . mkGame
  where label g = fmap intToDigit . tail . take 9 $ walkFrom g 1

part2 :: Int
part2 = product . drop 1 . take 3 . walkFrom (ntimes 10000000 play g) $ 1
  where g = mkGame (cups <> [10..1000000])
