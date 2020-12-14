module Day8 where

import           Data.Bifunctor (first)
import           Data.Either    (rights)
import           Data.Maybe     (listToMaybe)
import qualified Data.Vector    as V

import           Advent.Search

import           Computer

getInput :: FilePath -> IO Program
getInput = readProgram

part1 :: Program -> Either Int Int
part1 =  first _progAcc . loopOrTerminate

part2 :: Program -> Maybe Int
part2 = listToMaybe . rights . fmap loopOrTerminate . bruteforce
  where
    bruteforce :: Program -> [Program]
    bruteforce = fmap V.fromList . perturb f . V.toList
      where
        f (NOOP x) = [JMP x]
        f (JMP x)  = [NOOP x]
        f _        = []
