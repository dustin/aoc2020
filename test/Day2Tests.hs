module Day2Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day2

testPart1 :: Assertion
testPart1 = assertEqual "" 622 . part1 =<< getInput "input/day2"

testPart2 :: Assertion
testPart2 = assertEqual "" 263 . part2 =<< getInput "input/day2"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
