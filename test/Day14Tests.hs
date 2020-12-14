module Day14Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day14

testPart1 :: Assertion
testPart1 = assertEqual "" 11179633149677 . part1 =<< getInput "input/day14"

testPart2 :: Assertion
testPart2 = assertEqual "" 4822600194774 . part2 =<< getInput "input/day14"

testPart2ex :: Assertion
testPart2ex = assertEqual "" 208 . part2 =<< getInput "input/day14.ex2"

testMask :: Assertion
testMask = assertEqual "" 73 (applyMask (2, 64) 11)

tests :: [TestTree]
tests = [
  testCase "test mask" testMask,
  testCase "part1" testPart1,
  testCase "part2ex" testPart2ex,
  testCase "part2" testPart2
  ]
