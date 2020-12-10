module Day10Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day10

testPart1 :: Assertion
testPart1 = assertEqual "" 1700 . part1 =<< getInput "input/day10"

testPart2 :: Assertion
testPart2 = assertEqual "" 12401793332096 . part2 =<< getInput "input/day10"

testPart2' :: Assertion
testPart2' = assertEqual "" 12401793332096 . part2' =<< getInput "input/day10"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2,
  testCase "part2 (map)" testPart2'
  ]
