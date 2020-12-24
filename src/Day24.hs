module Day24 where

import           Control.DeepSeq (NFData (..), rwhnf)
import           Data.Foldable   (asum)
import           Data.Functor    (($>))
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import           Text.Megaparsec (endBy, many)

import           Advent.AoC
import           Advent.TwoD

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast
  deriving (Show, Enum, Bounded)

instance NFData Direction where rnf = rwhnf

type Input = [[Direction]]

-- Axial hex coordinates.
move :: Direction -> Point -> Point
move East (x,y)      = (x+1, y)
move SouthEast (x,y) = (x, y+1)
move SouthWest (x,y) = (x-1, y+1)
move West (x,y)      = (x-1, y)
move NorthWest (x,y) = (x, y-1)
move NorthEast (x,y) = (x+1, y-1)

around6 :: Point -> [Point]
around6 p = [move d p | d <- [minBound .. maxBound]]

parseDirection :: Parser Direction
parseDirection = asum ["e"  $> East, "se" $> SouthEast, "ne" $> NorthEast,
                       "w"  $> West, "sw" $> SouthWest, "nw" $> NorthWest]

parseInput :: Parser Input
parseInput = many parseDirection `endBy` "\n"

getInput :: FilePath -> IO Input
getInput = parseFile parseInput

blackTiles :: Input -> Set Point
blackTiles inp = Map.keysSet . Map.filter odd . Map.fromListWith (+) $ [(point,1::Int) | point <- points]
  where points = foldr move (0,0) <$> inp

part1 :: Input -> Int
part1 = length . blackTiles

play :: Ord k => (k -> [k]) -> Set k -> Set k
play ex w = keep <> new
  where
    counts = Map.fromListWith (+) [(k,1::Int) | k <- foldMap ex w]
    keep = Map.keysSet . Map.filter (\n -> n == 1 || n == 2) $ counts `Map.restrictKeys` w
    new = Map.keysSet . Map.filter (== 2) $ counts `Map.withoutKeys` w

part2 :: Input -> Int
part2 = length . ntimes 100 (play around6) . blackTiles
