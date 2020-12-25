module Day25 where

import           Data.Bifunctor (bimap)

part1 :: Int -> Int -> Int
part1 p t = head $ snd <$> filter ((== p) . fst) s
  where m n x = x * n `mod` 20201227
        s = iterate (bimap (m 7) (m t)) (1,1)
