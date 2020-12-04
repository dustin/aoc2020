module Day4Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day4

testPart1 :: Assertion
testPart1 = assertEqual "" 170 . part1 =<< getInput "input/day4"

testPart2 :: Assertion
testPart2 = assertEqual "" 103 . part2 =<< getInput "input/day4"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
