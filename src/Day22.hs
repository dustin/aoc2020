{-# LANGUAGE ViewPatterns #-}
-- There are edge cases that we don't need to consider
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day22 where

import           Control.Applicative        (liftA2)
import           Data.Bits                  (shiftL, (.|.))
import           Data.Foldable              (foldl', toList)
import qualified Data.IntSet                as Set
import           Data.Sequence              (Seq (..), (|>))
import qualified Data.Sequence              as Seq
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (digitChar)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

type Game = (Seq Int, Seq Int)

data Player = Player1 | Player2 deriving (Show, Eq)

parsePlayer :: Parser (Seq Int)
parsePlayer = Seq.fromList <$> (("Player " *> digitChar <* ":\n") *> (L.decimal `endBy` "\n"))

parseInput :: Parser Game
parseInput = liftA2 (,) (parsePlayer <* "\n") parsePlayer

getInput :: FilePath -> IO Game
getInput = parseFile parseInput

play :: Bool -> Game -> (Player, Seq Int)
play recurse = go mempty
  where
    go _ (p1, Empty) = (Player1, p1)
    go _ (Empty, p2) = (Player2, p2)
    go s (p1deck@(p1 :<| p1s), p2deck@(p2 :<| p2s))
      | Set.member sk s = (Player1, Empty)
      | recurse && length p1s >= p1 && length p2s >= p2 =
          let p1deck' = Seq.take p1 p1s
              p2deck' = Seq.take p2 p2s
              maxp1 = maximum p1deck'
              maxp2 = maximum p2deck'
          in
            if maxp1 > maxp2 && maxp1 > (p1 + p2)
            then go s' awin
            else
              case go mempty (p1deck', p2deck') of
                (Player1,_) -> go s' awin
                (Player2,_) -> go s' bwin
      | p1 > p2 = go s' awin
      | p2 > p1 = go s' bwin
      | otherwise = error "tie"

      where awin = (p1s |> p1 |> p2, p2s)
            bwin = (p1s, p2s |> p2 |> p1)
            s' = Set.insert sk s
            -- This is a more general case.  sim642's input requires more than just firsts+lasts+lens
            -- sk = foldl' (\o -> (+ ((shiftL o 5) + o))) 5381 ((p1deck |> 0) <> p2deck)
            -- Some people's input requires lens, mine doesn't.
            -- sk = foldl' (\o -> (.|. shiftL o 8)) 0 [length p1deck, length p2deck, p1, p2, t p1deck, t p2deck]
            sk = foldl' (\o -> (.|. shiftL o 8)) 0 [p1, p2, t p1deck, t p2deck]
              where t (_ :|> t') = t'

score :: Seq Int -> Int
score = sum . zipWith (*) [1..] . reverse . toList

part1 :: Game -> Int
part1 = score . snd . play False

part2 :: Game -> Int
part2 = score . snd . play True
