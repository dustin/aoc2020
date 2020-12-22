module Day22Tests where

import           Data.Bits             (complement)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day22

testPart1 :: Assertion
testPart1 = assertEqual "" 33403 . part1 =<< getInput "input/day22"

testPart2ex :: Assertion
testPart2ex = assertEqual "" 291 . part2 =<< getInput "input/day22.ex"

testPart2 :: Assertion
testPart2 = assertEqual "" 29177 . part2 =<< getInput "input/day22"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2ex" testPart2ex,
  testCase "part2" testPart2
  ]
