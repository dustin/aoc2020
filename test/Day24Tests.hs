module Day24Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day24

testPart1 :: Assertion
testPart1 = assertEqual ""  377. part1 =<< getInput "input/day24"

testPart2 :: Assertion
testPart2 = assertEqual "" 4231 . part2 =<< getInput "input/day24"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
