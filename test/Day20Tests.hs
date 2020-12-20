module Day20Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Advent.AoC

import           Day20

testPart1 :: Assertion
testPart1 = assertEqual "" 47213728755493 . part1 =<< getInput "input/day20"

testPart2 :: Assertion
testPart2 = assertEqual "" 0 . part2 =<< getInput "input/day20"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
