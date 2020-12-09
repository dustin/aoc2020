module Search where

import           Data.Foldable (toList)
import qualified Data.IntMap   as IntMap

-- | twosum via IntSet.
twosumOn :: Foldable t
         => (a -> Int)   -- ^ Representation function
         -> (Int -> Int) -- ^ Target function, typically (someVal -)
         -> t a          -- ^ stuff to search
         -> Maybe (a, a) -- ^ The target values
twosumOn r tf = go mempty . toList
  where
    go _ [] = Nothing
    go m (x:xs')
      | (tf (r x)) `IntMap.member` m = Just (x, m IntMap.! (tf (r x)))
      | otherwise = go (IntMap.insert (r x) x m) xs'

-- | twosum case where you have a list of ints and a specific target
-- you wish to find members that add up to.
twosum :: Foldable t => Int -> t Int -> Maybe (Int, Int)
twosum target = twosumOn id (target -)
