module Day1 where

import           Search

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input/day1"

part1 :: [Int] -> Int
part1 xs = let Just (a,b) = twosum 2020 xs in a*b

part2 :: [Int] -> Int
part2 xs = head [x * y * z | x <- xs, Just (y,z) <- [twosum (2020-x) xs]]
