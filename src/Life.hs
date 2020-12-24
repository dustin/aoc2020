module Life where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)

-- | One step of the game of life.
gameOfLife :: Ord k
           => (k -> [k])    -- ^ Neighbor function
           -> (Int -> Bool) -- ^ A cell stays alive if it has this many neighbors
           -> (Int -> Bool) -- ^ A cell becomes alive if it has this many neighbors
           -> Set k         -- ^ Current cells
           -> Set k
gameOfLife ex kf nf w = keep <> new
  where
    counts = Map.fromListWith (+) [(k,1::Int) | k <- foldMap ex w]
    keep   = Map.keysSet . Map.filter kf $ counts `Map.restrictKeys` w
    new    = Map.keysSet . Map.filter nf $ counts `Map.withoutKeys` w
