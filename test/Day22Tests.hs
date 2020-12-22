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

-- 15 games from http://ix.io/2JbX
testMany :: Assertion
testMany = assertEqual "" [34700,34361,33719,32509,34101,
                           33007,32327,32686,31397,34962,
                           31783,32716,33403,30782,33201] . fmap part2 =<< getInputs "input/day22.many"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2ex" $ testPart2For "input/day22.ex" 291,
  -- This fails with my simple/fast hand detection.
  testCase "part2 (sim642)" $ testPart2For "input/day22.sim642" 32317,
  testCase "part2" $ testPart2For "input/day22" 29177,
  testCase "part2.many" testMany
  ]
