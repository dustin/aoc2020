module Day5 where

import           Data.List      (foldl')
import           Data.Semigroup (Max (..), Min (..), Sum (..))
import qualified Data.Set       as Set

decode :: String -> Int
decode = foldl' (\o x -> o * 2 + fromEnum (x `elem` ['B', 'R'])) 0

getInput :: FilePath -> IO [Int]
getInput fn = fmap decode . lines <$> readFile fn

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 w = head . filter hasNeighbor . Set.toList $ missing
  where
    hasNeighbor s = Set.member (s + 1) allids && Set.member (s - 1) allids
    allids = Set.fromList w
    possible = Set.fromList [ r * 8 + c | r <- [1..126], c <- [0..7]]
    missing = possible `Set.difference` Set.fromList w

-- So, since all seats are encoded as numbers and we know the whole
-- range of populated seats, we can just add up all of seats that
-- should be there and subtract the value that we got to get the
-- missing seat number.
part2' :: [Int] -> Int
part2' i = sum [mn..mx] - s
  where (Min mn, Max mx, Sum s) = foldMap (\x -> (Min x, Max x, Sum x)) i
        -- sum [a..b] = (b(b+1)-a(a-1))/2
