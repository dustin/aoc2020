module Day3 where

import           Data.Coerce     (coerce)
import           Data.Foldable   (fold)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid     (Product (..), Sum (..))

import           Advent.AoC
import           Advent.TwoD

type World = Map Point (Sum Int)

getInput :: FilePath -> IO World
getInput fn = parseGrid (\case '#' -> 1; _ -> 0) <$> readFile fn

-- Count the trees along a given slope.
trees :: World -> (Int, Int) -> Sum Int
trees m (xoff, yoff) = fold [ at i | i <- [1 .. (maxy `div` yoff)] ]
  where
    ((maxx, maxy),_) = Map.findMax m
    at i = m Map.! (i*xoff `mod` (maxx+1), i*yoff)

part1 :: World -> Int
part1 w = getSum $ trees w (3,1)

part2 :: World -> Int
part2 w = getProduct . foldMap (coerce . trees w) $ [(1,1), (3,1), (5, 1), (7, 1), (1, 2)]
