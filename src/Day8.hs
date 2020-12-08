module Day8 where

import           Data.Either (fromLeft, rights)
import qualified Data.Vector as V

import           Computer

part1 :: Program -> Int
part1 = snd . fromLeft undefined . loopOrTerminate

getInput :: FilePath -> IO Program
getInput = readProgram

bruteforce :: Program -> [Program]
bruteforce = fmap V.fromList . go . V.toList
  where
    go :: [Instruction] -> [[Instruction]]
    go []                          = []
    go (i@(Instruction NOOP x):xs) = [Instruction JMP x:xs] <> fmap (i:) (go xs)
    go (i@(Instruction JMP x):xs)  = [Instruction NOOP x:xs] <> fmap (i:) (go xs)
    go (x:xs)                      = fmap (x:) (go xs)

part2 :: Program -> Int
part2 = head . rights . fmap loopOrTerminate . bruteforce
