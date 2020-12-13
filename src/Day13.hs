{-# LANGUAGE LambdaCase #-}

{-# Options_GHC -Wno-orphans #-}

module Day13 where

import           Control.Applicative              ((<|>))
import           Control.DeepSeq                  (NFData (..), rwhnf)
import           Data.Maybe                       (mapMaybe)
import           Math.NumberTheory.Moduli.Chinese
import           Text.Megaparsec                  (sepBy)
import           Text.Megaparsec.Char.Lexer       (decimal)


import           Advent.AoC

data Input n = Input {
  _earliest :: n
  , _buses  :: [Maybe n]
  } deriving Show

instance NFData (Input a) where rnf = rwhnf

parseInput :: Num n => Parser (Input n)
parseInput = Input <$> decimal <* "\n" <*> bus `sepBy` ","
  where bus = Nothing <$ "x" <|> Just <$> decimal

getInput :: Num n => FilePath -> IO (Input n)
getInput = parseFile parseInput

part1 :: Input Int -> Int
part1 Input{..} = bid * waiting
  where
    next i = (i - (_earliest `rem` i), i)
    (waiting, bid) = minimum $ mapMaybe (fmap next) _buses

part2 :: Input Integer -> Maybe Integer
part2 Input{..} = chineseRemainder bs
  where
    bs = mapMaybe f (zip _buses [0..])
    f (Just x, n) = Just (x-n `mod` x, x)
    f _           = Nothing

-- nshepperd
part2ns :: Input Integer -> Integer
part2ns Input{..} = go rules [0..]
  where
    go [] ts     = head ts
    go (r:rs) ts = go rs (accelerate (filter (solves r) ts))
    solves (dt,n) t = mod (t+dt) n == 0
    accelerate ts = let a:b:_ = ts in [a,b..]
    rules = [(dt, n) | (dt, Just n) <- zip [0..] _buses]
