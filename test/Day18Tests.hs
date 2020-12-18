module Day18Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Advent.AoC

import           Day18

testPart1 :: Assertion
testPart1 = assertEqual "" 1451467526514 . part1 =<< getInput flat "input/day18"

testPart2Ex :: Assertion
testPart2Ex = do
  let p = parseExprs plusFirst
  assertEqual "" 231 (part2 (parseLit p "1 + 2 * 3 + 4 * 5 + 6\n"))
  assertEqual "" 51 (part2 (parseLit p "1 + (2 * 3) + (4 * (5 + 6))\n"))
  assertEqual "" 46 (part2 (parseLit p "2 * 3 + (4 * 5)\n"))
  assertEqual "" 1445 (part2 (parseLit p "5 + (8 * 3 + 9 + 3 * 4 * 3)\n"))
  assertEqual "" 669060 (part2 (parseLit p "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))\n"))
  assertEqual "" 23340 (part2 (parseLit p "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2\n"))

testPart2 :: Assertion
testPart2 = assertEqual "" 224973686321527 . part2 =<< getInput plusFirst "input/day18"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2ex" testPart2Ex,
  testCase "part2" testPart2
  ]
