module Day6 where

import           Data.Word            (Word32)
import           Text.Megaparsec      (endBy, optional, sepBy, some)
import           Text.Megaparsec.Char (letterChar, spaceChar)

import           Advent.AoC
import           Advent.BitSet        (BitSet)
import qualified Advent.BitSet        as BitSet

type CharSet = BitSet Char Word32

getInput :: FilePath -> IO [[CharSet]]
getInput = parseFile (parseGroup `sepBy` "\n")
  where parseGroup = (BitSet.fromList ('a', 'z') <$> some letterChar) `endBy` optional spaceChar

part1 :: [[CharSet]] -> Int
part1 = sum . fmap (BitSet.length . foldr1 BitSet.union)

part2 :: [[CharSet]] -> Int
part2 = sum . fmap (BitSet.length . foldr1 BitSet.intersection)
