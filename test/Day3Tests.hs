module Day3Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day3

testPart1 :: Assertion
testPart1 = assertEqual "" 187 . part1 =<< getInput "input/day3"

testPart2 :: Assertion
testPart2 = assertEqual "" 4723283400 . part2 =<< getInput "input/day3"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
