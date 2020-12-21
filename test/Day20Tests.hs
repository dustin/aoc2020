module Day20Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Advent.AoC

import           Day20

testPart1 :: Assertion
testPart1 = assertEqual "" 47213728755493 . part1 =<< getInput "input/day20"

testPart2 :: Assertion
testPart2 = assertEqual "" 1599 . part2 =<< getInput "input/day20"

newtype AFrag = AFrag Fragment
  deriving (Show, Eq)

instance Arbitrary AFrag where
  arbitrary = do
    n <- (*2) <$> choose (1, 5) -- smallish even number
    AFrag <$> vectorOf n (vectorOf n (elements ['.', '#']))

prop_same :: (Show a, Eq a) => (a -> a) -> a -> Property
prop_same f = f >>= (===)

sideIdentity :: (MapEdges -> MapEdges) -> AFrag -> Property
sideIdentity f (AFrag frag) = prop_same f (sides frag)

fragIdentity :: (Fragment -> Fragment) -> AFrag -> Property
fragIdentity f (AFrag frag) = prop_same f frag

edgeCompare :: (MapEdges -> MapEdges, Fragment -> Fragment) -> AFrag -> Property
edgeCompare (mf, ff) (AFrag frag) = sides (ff frag) === mf (sides frag)

tests :: [TestTree]
tests = [
  testGroup "edges" [
      testProperty "flip X" $ sideIdentity (ntimes 2 flipEdgesX),
      testProperty "flip Y" $ sideIdentity (ntimes 2 flipEdgesY),
      testProperty "rotate" $ sideIdentity (ntimes 4 rotateEdges)
      ],

  testGroup "whole frags" [
      testProperty "flip X" $ fragIdentity (ntimes 2 flipFragX),
      testProperty "flip Y" $ fragIdentity (ntimes 2 flipFragY),
      testProperty "rotate" $ fragIdentity (ntimes 4 rotateFrag),
      testGroup "vs. edge" [
          testProperty "flip X" $ edgeCompare (flipEdgesX, flipFragX),
          testProperty "flip Y" $ edgeCompare (flipEdgesY, flipFragY),
          testProperty "rotate" $ edgeCompare (rotateEdges, rotateFrag)
          ]
      ],

  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
