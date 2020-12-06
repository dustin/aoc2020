module Day6 where

import qualified Data.Set     as Set
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

getInput :: FilePath -> IO [[Set.Set Char]]
getInput fn = do
  groups <- T.splitOn "\n\n" . T.strip <$> TIO.readFile fn
  let people = fmap (T.splitOn "\n") groups
  pure $ (fmap.fmap) (Set.fromList . T.unpack) people

part1 :: [[Set.Set Char]] -> Int
part1 =  sum . fmap Set.size . fmap (foldr1 Set.union)

part2 :: [[Set.Set Char]] -> Int
part2 = sum . fmap Set.size . fmap (foldr1 Set.intersection)
