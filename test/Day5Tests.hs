module Day5Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day5

testDecode :: Assertion
testDecode = assertEqual "" (70, 7) (decode "BFFFBBFRRR")

testPart1 :: Assertion
testPart1 = assertEqual "" 908 . part1 =<< getInput "input/day5"

testPart2 :: Assertion
testPart2 = assertEqual "" 619 . part2 =<< getInput "input/day5"

tests :: [TestTree]
tests = [
  testCase "decode" testDecode,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
