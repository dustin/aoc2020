module Day8 where

import           Data.Bifunctor (first)
import           Data.Either    (rights)
import           Data.Maybe     (listToMaybe)
import qualified Data.Vector    as V

import           Computer

getInput :: FilePath -> IO Program
getInput = readProgram

part1 :: Program -> Either Int Int
part1 = first snd . loopOrTerminate

part2 :: Program -> Maybe Int
part2 = listToMaybe . rights . fmap loopOrTerminate . bruteforce
  where
    bruteforce :: Program -> [Program]
    bruteforce = fmap V.fromList . go . V.toList
      where
        go :: [Instruction] -> [[Instruction]]
        go []                          = []
        go (i@(Instruction NOOP x):xs) = [Instruction JMP x:xs] <> fmap (i:) (go xs)
        go (i@(Instruction JMP x):xs)  = [Instruction NOOP x:xs] <> fmap (i:) (go xs)
        go (x:xs)                      = fmap (x:) (go xs)
