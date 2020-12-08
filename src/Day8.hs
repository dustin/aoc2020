module Day8 where

import           Data.Either   (rights)
import           Data.Maybe    (listToMaybe)
import qualified Data.Vector   as V

import           Advent.AoC
import           Advent.Search

import           Computer

getInput :: FilePath -> IO Program
getInput = readProgram

part1 :: Program -> Either ProgramState Int
part1 =  fmap snd . thrd . findCycle (fmap fst) . run

part2 :: Program -> Maybe Int
part2 = listToMaybe . rights . fmap loopOrTerminate . bruteforce
  where
    bruteforce :: Program -> [Program]
    bruteforce = fmap V.fromList . perturb f . V.toList
      where
        f (Instruction NOOP x) = [Instruction JMP x]
        f (Instruction JMP x)  = [Instruction NOOP x]
        f _                    = []
