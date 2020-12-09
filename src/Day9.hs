{-# LANGUAGE FlexibleContexts #-}

module Day9 where

import           Data.Maybe (listToMaybe)

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
part2 xs = minimum sub + maximum sub
  where (Just (l,h)) = listToMaybe [(snd a, snd b) | a <- sums, b <- sums, fst b - fst a == maxv]
        sums = zip (scanl (+) 0 xs) [0..]
        sub = take (h-l) $ drop l xs
        maxv = part1 xs
