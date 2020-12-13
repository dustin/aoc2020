{-# LANGUAGE LambdaCase #-}

{-# Options_GHC -Wno-orphans #-}

module Day13 where

import           Control.Applicative              ((<|>))
import           Control.DeepSeq                  (NFData (..), rwhnf)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (mapMaybe)
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
    next Nothing  = Nothing
    next (Just i) = let (q,r) = quotRem _earliest i in Just (q*i + if r == 0 then 0 else i, i)
    buses = Map.fromList (mapMaybe next _buses)
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
part2ns Input{..} = go rules [0..]
  where
    go [] ts     = head ts
    go (r:rs) ts = go rs (accelerate (filter (solves r) ts))
    solves (dt,n) t = mod (t+dt) n == 0
    accelerate ts = let a:b:_ = ts in [a,b..]
    rules = [(dt, n) | (dt, Just n) <- zip [0..] _buses]
