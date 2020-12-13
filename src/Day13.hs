{-# LANGUAGE LambdaCase #-}

{-# Options_GHC -Wno-orphans #-}

module Day13 where

import           Control.Applicative              ((<|>))
import           Control.DeepSeq                  (NFData (..), rwhnf)
import           Data.Maybe                       (mapMaybe)
import           Data.Tuple                       (swap)
import           Math.NumberTheory.Moduli.Chinese
import           Text.Megaparsec                  (sepBy)
import           Text.Megaparsec.Char.Lexer       (decimal)


import           Advent.AoC

data Input n = Input {
  _earliest :: n
  , _buses  :: [(n, n)]
  } deriving Show

instance NFData (Input a) where rnf = rwhnf

parseInput :: (Enum n, Num n) => Parser (Input n)
parseInput = do
  _earliest <- decimal <* "\n"
  buses <- bus `sepBy` ","
  let _buses = mapMaybe f $ zip buses [0..]
  pure Input{..}
  where
    bus = Nothing <$ "x" <|> Just <$> decimal
    f (Nothing, _) = Nothing
    f (Just o, x)  = Just (o, x)

getInput :: (Enum n, Num n) => FilePath -> IO (Input n)
getInput = parseFile parseInput

part1 :: Input Int -> Int
part1 Input{..} = bid * waiting
  where
    next (i,_) = (i - (_earliest `rem` i), i)
    (waiting, bid) = minimum $ fmap next _buses

part2 :: Input Integer -> Maybe Integer
part2 Input{..} = chineseRemainder (fmap (\(x, n) -> (x-n `mod` x, x)) _buses)

-- nshepperd
part2ns :: Input Integer -> Integer
part2ns Input{..} = go (fmap swap _buses) [0..]
  where
    go [] ts     = head ts
    go (r:rs) ts = go rs (accelerate (filter (solves r) ts))
    solves (dt,n) t = mod (t+dt) n == 0
    accelerate ts = let a:b:_ = ts in [a,b..]
