{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-# Options_GHC -Wno-orphans #-}

module Day13 where

import           Control.Applicative              ((<|>))
import           Control.DeepSeq                  (NFData (..), rwhnf)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, mapMaybe)
import           Math.NumberTheory.Moduli.Chinese
import           Text.Megaparsec                  (sepBy)
import           Text.Megaparsec.Char.Lexer       (decimal)


import           Advent.AoC

data Input = Input {
  _earliest :: Integer
  , _buses  :: [Maybe Integer]
  } deriving Show

instance NFData Input where rnf = rwhnf

parseInput :: Parser Input
parseInput = Input <$> decimal <* "\n" <*> bus `sepBy` ","
  where bus = Nothing <$ "x" <|> Just <$> decimal

getInput :: FilePath -> IO Input
getInput = parseFile parseInput

part1 :: Input -> Integer
part1 Input{..} = bid * waiting
  where
    buses = Map.unions (expand <$> catMaybes _buses)
    expand x = Map.fromList [(y, x) | y <- [x, x + x .. _earliest + (2*x)], y >= _earliest]
    Just (nt, bid) = Map.lookupGE _earliest buses
    waiting = nt - _earliest

part2 :: Input -> Maybe Integer
part2 Input{..} = chineseRemainder bs
  where
    bs = mapMaybe f (zip _buses [0..])
    f (Just x, n) = Just (x-n `mod` x, x)
    f _           = Nothing

-- nshepperd
part2ns :: Input -> Integer
part2ns Input{..} = go r1 rules
  where
    go (t0, _) [] = t0
    go (t0, t1) ((dt,n):rs) = let a:b:_ = [t | t <- [t0,t1..], mod (t+dt) n == 0] in
                                go (a,b) rs
    (r1:rules) = [(dt, n) | (dt, Just n) <- zip [0..] _buses]
