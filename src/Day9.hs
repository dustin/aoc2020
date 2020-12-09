module Day9 where

import           Data.Maybe     (isJust)
import           Data.Semigroup (Max (..), Min (..))

import           Search

getInput :: FilePath -> IO [Int]
getInput = fmap (fmap read . words) . readFile

part1' :: Int -> [Int] -> Int
part1' size xs = let (h,t) = splitAt size xs in go h t
  where
    go _ [] = 0
    go prev (x:xs')
      | matches x prev = go (drop 1 prev <> [x]) xs'
      | otherwise = x
    matches x ns = isJust (twosum x ns)

part1 :: [Int] -> Int
part1 = part1' 25

part2 :: [Int] -> Int
part2 xs = subMin + subMax
  where Just ((_,h),(_,l)) = twosumOn fst (subtract maxv) sums
        sums = zip (scanl (+) 0 xs) [0..]
        (Min subMin, Max subMax) = foldMap (\x -> (Min x, Max x)) . take (h-l) $ drop l xs
        maxv = part1 xs
