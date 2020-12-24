module Day24 where

import           Control.Applicative ((<|>))

import           Control.DeepSeq     (NFData (..), rwhnf)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import           Text.Megaparsec     (endBy, many, try)

import           Advent.AoC
import           Advent.TwoD

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast
  deriving (Show, Enum, Bounded)

instance NFData Direction where rnf = rwhnf

type Input = [[Direction]]

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
parseDirection = (East <$ "e")
                 <|> (SouthEast <$ try "se")
                 <|> (SouthWest <$ "sw")
                 <|> (West <$ "w")
                 <|> (NorthWest <$ try "nw")
                 <|> (NorthEast <$ "ne")

parseLine :: Parser [Direction]
parseLine = many parseDirection

parseInput :: Parser Input
parseInput = parseLine `endBy` "\n"

getInput :: FilePath -> IO Input
getInput = parseFile parseInput

findTile :: Point -> [Direction] -> Point
findTile p []     = p
findTile p (d:ds) = findTile (move d p) ds

data Color = Black | White deriving (Show, Eq)

countTiles :: Input -> Map Point Int
countTiles inp = Map.fromListWith (+) [(point,1) | point <- points]
  where points = findTile (0,0) <$> inp

flipTiles :: Input -> Map Point Int
flipTiles = Map.filter odd . countTiles

part1 :: Input -> Int
part1 = length . flipTiles

type Game = Set Point

play :: Ord k => (k -> [k]) -> Set k -> Set k
play ex w = keep <> new
  where
    counts = Map.fromListWith (+) [(k,1::Int) | k <- foldMap ex w]
    keep = Map.keysSet . Map.filter (\n -> n == 1 || n == 2) $ counts `Map.restrictKeys` w
    new = Map.keysSet . Map.filter (== 2) $ counts `Map.withoutKeys` w

part2 :: Input -> Int
part2 inp = length (ntimes 100 (play around6) g)
  where g = Map.keysSet . fmap (const Black) . Map.filter odd . countTiles $ inp
