module Day25Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day25

testPart1 :: Assertion
testPart1 = assertEqual "" 9620012 (part1 3418282 8719412)

tests :: [TestTree]
tests = [
  testCase "part1" testPart1
  ]
