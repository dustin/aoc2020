module Day9Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day9

testPart1 :: Assertion
testPart1 = assertEqual "" 248131121 . part1 =<< getInput "input/day9"

testPart2 :: Assertion
testPart2 = assertEqual "" 31580383 . part2 =<< getInput "input/day9"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
