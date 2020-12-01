module Day1Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day1

testPart1 :: Assertion
testPart1 = assertEqual "" 1009899 . part1 =<< getInput

testPart2 :: Assertion
testPart2 = assertEqual "" 44211152 . part2 =<< getInput

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
