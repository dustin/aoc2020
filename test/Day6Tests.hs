module Day6Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day6

testPart1 :: Assertion
testPart1 = assertEqual "" 7283 . part1 =<< getInput "input/day6"

testPart2 :: Assertion
testPart2 = assertEqual "" 3520 . part2 =<< getInput "input/day6"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
