module Day6 where

import qualified Data.Set     as Set
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

getInput :: FilePath -> IO [[Set.Set Char]]
getInput = fmap parse . TIO.readFile
  where parse = fmap (fmap (Set.fromList . T.unpack) . T.splitOn "\n") . T.splitOn "\n\n" . T.strip

part1 :: [[Set.Set Char]] -> Int
part1 = sum . fmap (Set.size . foldr1 Set.union)

part2 :: [[Set.Set Char]] -> Int
part2 = sum . fmap (Set.size . foldr1 Set.intersection)
