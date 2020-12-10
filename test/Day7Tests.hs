module Day7Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day7

testPart1 :: Assertion
testPart1 = assertEqual "" (Just 254) . part1 =<< getInput "input/day7"

testPart2 :: Assertion
testPart2 = assertEqual "" 6006 . part2 =<< getInput "input/day7"

testPart2' :: Assertion
testPart2' = assertEqual "" 6006 . part2' =<< getInput "input/day7"

testPart2l :: Assertion
testPart2l = assertEqual "" 6006 . part2l =<< getInput "input/day7"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2,
  testCase "part2'" testPart2',
  testCase "part2 lmap" testPart2l
  ]
