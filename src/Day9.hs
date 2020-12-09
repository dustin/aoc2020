{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Disabled incomplete patterns for part1' since I'm not checking for
-- empty sequences because there shouldn't be any and I'm OK with a
-- crash if I'm wrong.

module Day9 where

import           Data.Foldable  (toList)
import           Data.List      (tails)
import           Data.Semigroup (Max (..), Min (..))
import qualified Data.Sequence  as Seq

import           Search

getInput :: FilePath -> IO [Int]
getInput = fmap (fmap read . words) . readFile

part1' :: Int -> [Int] -> Int
part1' size xs = ans . dropWhile match $ windows
  where
    ans ((_ Seq.:|> a):_) = a
    windows = Seq.fromList . take (size+1) <$> tails xs
    match (w Seq.:|> t) = (not.null) [() | x:ys <- tails (toList w), y <- ys, x + y == t]

part1 :: [Int] -> Int
part1 = part1' 25

part2' :: Int -> [Int] -> Int
part2' maxv xs = subMin + subMax
  where Just ((_,h),(_,l)) = twosumOn fst (subtract maxv) sums
        sums = zip (scanl (+) 0 xs) [0..]
        (Min subMin, Max subMax) = foldMap (\x -> (Min x, Max x)) . take (h-l) $ drop l xs

part2 :: [Int] -> Int
part2 xs = part2' (part1 xs) xs
