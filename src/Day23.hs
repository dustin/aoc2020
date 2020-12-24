{-# LANGUAGE FlexibleContexts #-}
module Day23 where

import qualified Data.Array.MArray  as MA
import qualified Data.Array.ST      as MA
import qualified Data.Array.Unboxed as A
import           Data.Char          (intToDigit)
import           Data.List          (sort, unfoldr)

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
    d3next = _poses A.! d3last
    destPos = _poses A.! dest
    _poses' = _poses A.// [(_pos, d3next), (dest, head next3), (d3last, destPos)]
    guessSeq 0 = guessSeq (snd . A.bounds $ _poses)
    guessSeq n
      | elem n next3 = guessSeq (n - 1)
      | otherwise = n

playn :: Int -> Game -> Game
playn n = ntimes n play

playnST :: Int -> [Int] -> Game
playnST n ls = Game 1 $ MA.runSTUArray $ do
  let gs = sort . zip ls $ tail (cycle ls)
  ma <- MA.newListArray (1, maximum ls) (fmap snd gs)
  play' n ma (head ls)

  where
    play' 0 ma _ = pure ma
    play' !todo ma pos = do
      (_,maxv) <- MA.getBounds ma
      n3@(na,_,nc) <- getNext3 pos

      let dest = guessSeq (pos - 1) maxv n3
          d3last = nc
      destPos <- MA.readArray ma dest
      d3next <- MA.readArray ma d3last

      -- Fix up pointers
      MA.writeArray ma pos d3next
      MA.writeArray ma dest na
      MA.writeArray ma d3last destPos

      play' (pred todo) ma d3next

        where
          guessSeq 0 m r = guessSeq m m r
          guessSeq n' m r@(na,nb,nc)
            | n' == na || n' == nb || n' == nc = guessSeq (n' - 1) m r
            | otherwise = n'

          getNext3 p = do
            a <- MA.readArray ma p
            b <- MA.readArray ma a
            c <- MA.readArray ma b
            pure (a,b,c)

showGame :: Game -> [Int]
showGame g@Game{..} = take 9 $ walkFrom _pos g

part1 :: Int -> [Int] -> String
part1 n = label . playnST n
  where label = fmap intToDigit . tail . take 9 . walkFrom 1

part2 :: [Int] -> Int
part2 = product . drop 1 . take 3 . walkFrom 1 . playnST 10000000 . (<>[10..1000000])
