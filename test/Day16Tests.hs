module Day16Tests where

import           Data.Bits             (complement)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day16

testPart1 :: Assertion
testPart1 = assertEqual "" 22073 . part1 =<< getInput "input/day16"

testValidArrangements :: Assertion
testValidArrangements = do
  inp <- getInput "input/day16.ex2"
  assertEqual "" ["row", "class", "seat"] (fmap _fieldName $ validArrangements inp)

testPart2 :: Assertion
testPart2 = assertEqual "" 1346570764607 . part2 =<< getInput "input/day16"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "arrangements" testValidArrangements,
  testCase "part2" testPart2
  ]
