module Day22 where

import           Control.Applicative        (liftA2)
import qualified Data.Set                   as Set
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (digitChar)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

type Game = ([Int], [Int])

parsePlayer :: Parser [Int]
parsePlayer = ("Player " *> digitChar <* ":\n") *> (L.decimal `endBy` "\n")

parseInput :: Parser Game
parseInput = liftA2 (,) (parsePlayer <* "\n") parsePlayer

getInput :: FilePath -> IO Game
getInput = parseFile parseInput

play :: Game -> Either (Int,[Int]) Game
play ([], p2) = Left (2, p2)
play (p1, []) = Left (1, p1)
play (p1:p1s, p2:p2s)
  | p1 > p2 = Right (p1s <> [p1,p2], p2s)
  | p2 > p1 = Right (p1s, p2s <> [p2,p1])
  | otherwise = error "tie"

type PlayFun a = Game -> Either (Int, [Int]) Game

playOut :: PlayFun a -> Game -> Either (Int,[Int]) Game
playOut pf = go
  where go hands = pf hands >>= go

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

part1 :: Game -> Int
part1 = either (score . snd) (const 0) . playOut play

play2 :: Game -> Either (Int,[Int]) Game
play2 = go mempty
  where
    go _ ([], p2) = Left (2, p2)
    go _ (p1, []) = Left (1, p1)
    go s (a@(p1:p1s), b@(p2:p2s))
      | Set.member (a,b) s = Left (1,[])
      | length p1s >= p1 && length p2s >= p2 =
          case go mempty ((take p1 p1s), (take p2 p2s)) of
            Left (1,_) -> go s' awin
            Left (2,_) -> go s' bwin
            _          -> error "wtf"
      | p1 > p2 = go s' awin
      | p2 > p1 = go s' bwin
      | otherwise = error "tie"

      where awin = (p1s <> [p1,p2], p2s)
            bwin = (p1s, p2s <> [p2,p1])
            s' = Set.insert (a,b) s


part2 :: Game -> Int
part2 = either (score.snd) (const 0) . play2
