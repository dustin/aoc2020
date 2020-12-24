module Day24 where

import           Control.Applicative ((<|>))

import           Data.Foldable       (fold)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Text.Megaparsec     (endBy, many, try)

import           Advent.AoC
import           Advent.Search
import           Advent.TwoD

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast
  deriving (Show, Enum, Bounded)

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

gol6 :: Game -> Game
gol6 g = Set.fromList . foldMap (\p -> rules (Set.member p g) p) $ relevant
  where
    relevant = Set.toList (g <> fold [ Set.fromList (around6 p) | p <- Set.toList g ])
    rules True p  = let nc = ncount p in  if nc == 1 || nc == 2 then [p] else []
    rules False p = if ncount p == 2 then [p] else []

    ncount = countIf (`Set.member` g) . around6

part2 :: Input -> Int
part2 inp = length (ntimes 100 gol6 g)
  where g = Map.keysSet . fmap (const Black) . Map.filter odd . countTiles $ inp
