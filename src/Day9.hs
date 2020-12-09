{-# LANGUAGE FlexibleContexts #-}

module Day9 where

import           Data.Coerce    (coerce)
import           Data.List      (tails)
import           Data.Maybe     (catMaybes)
import           Data.Semigroup (Max (..), Min (..))

getInput :: FilePath -> IO [Int]
getInput = fmap (fmap read . words) . readFile

part1' :: Int -> [Int] -> Int
part1' size xs = let (h,t) = splitAt size xs in go h t
  where
    go _ [] = 0
    go prev (x:xs')
      | matches x prev = go (drop 1 prev <> [x]) xs'
      | otherwise = x
    matches x ns = (not.null) [() | a <- ns, b <- ns, a + b == x]

part1 :: [Int] -> Int
part1 = part1' 25

part2 :: [Int] -> Int
part2 xs = ans . head . catMaybes $ sums (part1 xs) <$> tails xs
  where ans (l,h) = l + h
        sums _ [] = Nothing
        sums maxv xs'@(x:_) = go 0 (Min x, Max x) xs'
          where
            go _ _ [] = Nothing
            go n r (x':xs'')
              | n + x' == maxv = Just (coerce r)
              | n + x' > maxv = Nothing
              | otherwise = go (n + x') (r <> (Min x', Max x')) xs''
