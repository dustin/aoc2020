module Day13Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day13

testPart1ex :: Assertion
testPart1ex = assertEqual "" 295 . part1 =<< getInput "input/day13.ex"

testPart1 :: Assertion
testPart1 = assertEqual "" 3997 . part1 =<< getInput "input/day13"

testPart2ex :: Assertion
testPart2ex = assertEqual "" 1068781 . part2ns =<< getInput "input/day13.ex"

testPart2ns :: Assertion
testPart2ns = assertEqual "" 500033211739354 . part2ns =<< getInput "input/day13"

tests :: [TestTree]
tests = [
  testCase "part1ex" testPart1ex,
  testCase "part1" testPart1,
  testCase "part2ex" testPart2ex,
  testCase "part2ns" testPart2ns
  ]
