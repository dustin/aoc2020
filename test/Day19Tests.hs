module Day19Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Advent.AoC

import           Day19

testPart1 :: Assertion
testPart1 = assertEqual "" 299 . part1 =<< getInput "input/day19"

testPart2 :: Assertion
testPart2 = assertEqual "" 414 . part2 =<< getInput "input/day19"

testPart2Small :: Assertion
testPart2Small = assertEqual "" 1 . part2 =<< getInput "input/day19.small"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2 small" testPart2Small,
  testCase "part2" testPart2
  ]
