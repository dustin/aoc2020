{-# LANGUAGE ViewPatterns #-}
-- There are edge cases that we don't need to consider

module Day22 where

import           Control.Applicative        (liftA2)
import           Data.Bits                  (shiftL)
import           Data.Foldable              (foldl', toList)
import qualified Data.IntSet                as Set
import           Data.Sequence              (Seq (..), (|>))
import qualified Data.Sequence              as Seq
import           Text.Megaparsec            (endBy, many)
import           Text.Megaparsec.Char       (digitChar, space)
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

-- For the "many" input where we're testing multiple games.
getInputs :: FilePath -> IO [Game]
getInputs = parseFile (many (L.lexeme space parseInput))

play :: Bool -> Game -> (Player, Seq Int)
play recurse = go mempty
  where
    go _ (p1, Empty) = (Player1, p1)
    go _ (Empty, p2) = (Player2, p2)
    go s (p1deck@(p1 :<| p1s), p2 :<| p2s)
      | Set.member sk s = (Player1, Empty)
      | recurse && length p1s >= p1 && length p2s >= p2 =
          let p1deck' = Seq.take p1 p1s
              p2deck' = Seq.take p2 p2s
              maxp1 = maximum p1deck'
              maxp2 = maximum p2deck'
          in go s' $ case () of _ -- supercard, followed by recursion
                                  | maxp1 > maxp2 && maxp1 > (p1 + p2)   -> awin
                                  | p1won $ go mempty (p1deck', p2deck') -> awin
                                  | otherwise                            -> bwin
      | p1 > p2 = go s' awin -- part1 awin
      | p2 > p1 = go s' bwin -- part1 bwin
      | otherwise = error "tie"

      where awin = (p1s |> p1 |> p2, p2s)
            bwin = (p1s, p2s |> p2 |> p1)
            p1won (Player1,_) = True
            p1won _           = False
            s' = Set.insert sk s
            -- Seems to work for many to just consider the first deck
            sk = foldl' (\o -> (+ ((shiftL o 5) + o))) 5381 p1deck

score :: Seq Int -> Int
score = sum . zipWith (*) [1..] . reverse . toList

part1 :: Game -> Int
part1 = score . snd . play False

part2 :: Game -> Int
part2 = score . snd . play True
