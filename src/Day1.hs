module Day1 where

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input/day1"

part1 :: [Int] -> Int
part1 xs = head [x * y | x <- xs, y <- xs, x + y == 2020 ]

part2 :: [Int] -> Int
part2 xs = head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020 ]
