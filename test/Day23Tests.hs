module Day23Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Data.Sequence     (Seq (..))
import qualified Data.Sequence     as Seq


import           Advent.AoC

import           Day23

cupsEx :: Seq Int
cupsEx = Seq.fromList [3,8,9,1,2,5,4,6,7]

testPart1 :: Assertion
testPart1 = do
  assertEqual "ex*10" "92658374" (label $ ntimes 10 (play 9) cupsEx)
  assertEqual "ex*100" "67384529" (label $ ntimes 100 (play 9) cupsEx)
  assertEqual "my*100" "32897654" part1

testPart2 :: Assertion
testPart2 = assertEqual "" 186715244496 part2

tests :: [TestTree]
tests = [
  testCase "part1" testPart1
  -- testCase "part2" testPart2
  ]
