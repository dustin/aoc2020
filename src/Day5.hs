module Day5 where

import qualified Data.Set as Set

decode :: String -> (Int, Int)
decode = go 0 127
  where
    go l h ('F':xs) = go l (l + ((h - l) `div` 2)) xs
    go l h ('B':xs) = go (h - ((h - l) `div` 2)) h xs
    go l _ xs       = go' l 0 7 xs

    go' r l h ('L':xs) = go' r l (l + ((h - l) `div` 2)) xs
    go' r l h ('R':xs) = go' r (h - ((h - l) `div` 2)) h xs
    go' r l _ _        = (r, l)

seatID :: (Int, Int) -> Int
seatID (r,c) = r * 8 + c

getInput :: FilePath -> IO [(Int, Int)]
getInput fn = fmap decode . lines <$> readFile fn

part1 :: [(Int, Int)] -> Int
part1 = maximum . fmap seatID

part2 :: [(Int, Int)] -> Int
part2 w = seatID . head . filter hasNeighbor . Set.toList $ missing
  where
    hasNeighbor s = Set.member (seatID s + 1) allids && Set.member (seatID s - 1) allids
    allids = Set.fromList (seatID <$> w)
    possible = Set.fromList [(r,c) | r <- [1..126], c <- [0..7]]
    missing = possible `Set.difference` Set.fromList w
