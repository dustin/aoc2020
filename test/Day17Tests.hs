module Day17Tests where

import           Data.Bits             (complement)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day17

testPart1 :: Assertion
testPart1 = assertEqual "" 265 . part1 =<< getInput "input/day17"

testPart1Ex :: Assertion
testPart1Ex = assertEqual "" 112 . part1 =<< getInput "input/day17.ex"

testPart2Ex :: Assertion
testPart2Ex = assertEqual "" 848 . part2 =<< getInput "input/day17.ex"

testPart2 :: Assertion
testPart2 = assertEqual "" 1936 . part2 =<< getInput "input/day17"

tests :: [TestTree]
tests = [
  testCase "part1ex" testPart1Ex,
  testCase "part2ex" testPart2Ex,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
