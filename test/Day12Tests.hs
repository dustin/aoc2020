module Day12Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day12

testPart1 :: Assertion
testPart1 = assertEqual "" 757 . part1 =<< getInput "input/day12"

testPart2ex :: Assertion
testPart2ex = assertEqual "" 286 . part2 =<< getInput "input/day12.ex"

testPart2 :: Assertion
testPart2 = assertEqual "" 51249 . part2 =<< getInput "input/day12"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2ex" testPart2ex,
  testCase "part2" testPart2
  ]
