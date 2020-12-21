module Day21Tests where

import           Data.Bits             (complement)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day21

testPart1 :: Assertion
testPart1 = assertEqual "" 2436 . part1 =<< getInput "input/day21"

testPart2 :: Assertion
testPart2 = assertEqual "" "dhfng,pgblcd,xhkdc,ghlzj,dstct,nqbnmzx,ntggc,znrzgs" . part2 =<< getInput "input/day21"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
