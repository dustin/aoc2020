module Day10Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day10

testPart1 :: Assertion
testPart1 = assertEqual "" 1700 . part1 =<< getInput "input/day10"

testPart2 :: Assertion
testPart2 = assertEqual "" 12401793332096 . part2 =<< getInput "input/day10"

testPart2' :: Assertion
testPart2' = assertEqual "" 12401793332096 . part2' =<< getInput "input/day10"

testPart2trib :: Assertion
testPart2trib = assertEqual "" 12401793332096 . part2trib =<< getInput "input/day10"

testPart2gl :: Assertion
testPart2gl = assertEqual "" 12401793332096 . part2gl =<< getInput "input/day10"

testPart2h :: Assertion
testPart2h = assertEqual "" 12401793332096 . part2h =<< getInput "input/day10"

testPart2span :: Assertion
testPart2span = assertEqual "" 12401793332096 . part2span =<< getInput "input/day10"


testArrangements :: Assertion
testArrangements = assertEqual "" [[19,16,15,12,10,7,4,1,0],
                                   [19,16,15,12,10,7,5,4,1,0],
                                   [19,16,15,12,10,7,6,4,1,0],
                                   [19,16,15,12,11,10,7,4,1,0],
                                   [19,16,15,12,10,7,6,5,4,1,0],
                                   [19,16,15,12,11,10,7,5,4,1,0],
                                   [19,16,15,12,11,10,7,6,4,1,0],
                                   [19,16,15,12,11,10,7,6,5,4,1,0]
                                  ] . arrangements =<< getInput "input/day10.ex"

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2,
  testCase "part2 (map)" testPart2',
  testCase "part2 (trib)" testPart2trib,
  testCase "part2 (glguy)" testPart2gl,
  testCase "part2 (hanqing)" testPart2h,
  testCase "part2 (span)" testPart2span,
  testCase "arrangements" testArrangements
  ]
