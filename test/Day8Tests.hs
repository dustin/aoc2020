module Day8Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day8

testPart1 :: Assertion
testPart1 = assertEqual "" 1489 . part1 =<< getInput "input/day8"

testPart2 :: Assertion
testPart2 = assertEqual "" 1539 . part2 =<< getInput "input/day8"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
