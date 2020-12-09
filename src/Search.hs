{-# LANGUAGE BangPatterns #-}

module Search where

import           Data.Foldable      (toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet
import           Data.List          (tails)
import           Data.Maybe         (listToMaybe)

-- | twosum via IntSet.
twosumOn :: Foldable t
         => (a -> Int)   -- ^ Representation function
         -> (Int -> Int) -- ^ Target function, typically (someVal -)
         -> t a          -- ^ stuff to search
         -> Maybe (a, a) -- ^ The target values
twosumOn r tf = go mempty . toList
  where
    go _ [] = Nothing
    go m (!x:xs')
      | (tf (r x)) `IntMap.member` m = Just (x, m IntMap.! (tf (r x)))
      | otherwise = go (IntMap.insert (r x) x m) xs'

-- | twosum case where you have a list of ints and a specific target
-- you wish to find members that add up to.
twosum :: Foldable t => Int -> t Int -> Maybe (Int, Int)
twosum !target = go mempty . toList
  where
    go _ [] = Nothing
    go s (!x:xs)
      | (target - x) `IntSet.member` s = Just (x, target - x)
      | otherwise = go (IntSet.insert x s) xs

-- | A dumb implementation of twosum that works better on smaller lists.
twodumb :: Foldable t => Int -> t Int -> Maybe (Int, Int)
twodumb !target xs = listToMaybe [ (x,y) | x:ys <- tails (toList xs), y <- ys, x + y == target]
