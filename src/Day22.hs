module Day22 where

import           Control.Applicative        (liftA2)
import qualified Data.Set                   as Set
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (digitChar)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

type Game = ([Int], [Int])

data Player = Player1 | Player2 deriving (Show, Eq)

parsePlayer :: Parser [Int]
parsePlayer = ("Player " *> digitChar <* ":\n") *> (L.decimal `endBy` "\n")

parseInput :: Parser Game
parseInput = liftA2 (,) (parsePlayer <* "\n") parsePlayer

getInput :: FilePath -> IO Game
getInput = parseFile parseInput

play :: Bool -> Game -> (Player, [Int])
play recurse = go mempty
  where
    go _ (p1, []) = (Player1, p1)
    go _ ([], p2) = (Player2, p2)
    go s (p1:p1s, p2:p2s)
      | Set.member sk s = (Player1, [])
      | recurse && length p1s >= p1 && length p2s >= p2 =
          case go mempty ((take p1 p1s), (take p2 p2s)) of
            (Player1,_) -> go s' awin
            (Player2,_) -> go s' bwin
      | p1 > p2 = go s' awin
      | p2 > p1 = go s' bwin
      | otherwise = error "tie"

      where awin = (p1s <> [p1,p2], p2s)
            bwin = (p1s, p2s <> [p2,p1])
            s' = Set.insert sk s
            sk = (p1, last (p1:p1s), p2, last (p2:p2s))

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

part1 :: Game -> Int
part1 = score . snd . play False

part2 :: Game -> Int
part2 = score . snd . play True
