module Day15Tests where

import           Data.Bits             (complement)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day15

testPart1Ex :: Assertion
testPart1Ex = do
  assertEqual "" [0, 3, 6, 0, 3, 3, 1, 0, 4, 0] (take 10 $ game [0,3,6])
  assertEqual "0,3,6" 436 (part1 [0, 3, 6])
  assertEqual "1,3,2" 1 (part1 [1,3,2])
  assertEqual "2,1,3" 10 (part1 [2,1,3])
  assertEqual "1,2,3" 27 (part1 [1,2,3])
  assertEqual "2,3,1" 78 (part1 [2,3,1])
  assertEqual "3,2,1" 438 (part1 [3,2,1])
  assertEqual "3,1,2" 1836 (part1 [3,1,2])

testPart1 :: Assertion
testPart1 = assertEqual "" 614 (part1 myInput)

testPart2Ex :: Assertion
testPart2Ex = do
  assertEqual "0,3,6" 175594 (part2 [0, 3, 6])
  assertEqual "1,3,2" 2578 (part2 [1,3,2])
  assertEqual "2,1,3" 3544142 (part2 [2,1,3])
  assertEqual "1,2,3" 261214 (part2 [1,2,3])
  assertEqual "2,3,1" 6895259 (part2 [2,3,1])
  assertEqual "3,2,1" 18 (part2 [3,2,1])
  assertEqual "3,1,2" 362 (part2 [3,1,2])

testPart2 :: Assertion
testPart2 = assertEqual "" 1065 (part2 myInput)

tests :: [TestTree]
tests = [
  testCase "part1ex" testPart1Ex,
  testCase "part1" testPart1,
  -- testCase "part2ex" testPart2Ex,
  testCase "part2" testPart2
  ]
