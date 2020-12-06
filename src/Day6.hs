module Day6 where

import qualified Data.Set             as Set
import           Text.Megaparsec      (endBy, optional, sepBy, some)
import           Text.Megaparsec.Char (letterChar, spaceChar)

import           Advent.AoC

getInput :: FilePath -> IO [[Set.Set Char]]
getInput = parseFile (parseGroup `sepBy` "\n")
  where parseGroup = (Set.fromList <$> some letterChar) `endBy` optional spaceChar

part1 :: [[Set.Set Char]] -> Int
part1 = sum . fmap (Set.size . foldr1 Set.union)

part2 :: [[Set.Set Char]] -> Int
part2 = sum . fmap (Set.size . foldr1 Set.intersection)
