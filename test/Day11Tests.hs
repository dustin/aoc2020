module Day11Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day11

testPart1 :: Assertion
testPart1 = assertEqual "" 2338 . part1 =<< getInput "input/day11"

testPart2 :: Assertion
testPart2 = assertEqual "" 2134 . part2 =<< getInput "input/day11"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
