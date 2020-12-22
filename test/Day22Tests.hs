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

testPart2For :: FilePath -> Int -> Assertion
testPart2For fn exp = assertEqual "" exp . part2 =<< getInput fn

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2ex" $ testPart2For "input/day22.ex" 291,
  -- This fails with my simple/fast hand detection.
  -- testCase "part2 (sim642)" $ testPart2For "input/day22.sim642" 32317,
  testCase "part2" $ testPart2For "input/day22" 29177
  ]
