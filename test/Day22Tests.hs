module Day22Tests where

import           Control.Applicative   (liftA2)
import           Data.Foldable         (toList)
import           Data.Sequence         (Seq (..), (|>))
import qualified Data.Sequence         as Seq
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

instance Arbitrary Game where
  arbitrary = uncurry Game . Seq.splitAt 5 . Seq.fromList <$> shuffle [1..10]
  shrink (Game l1 l2) = [Game (Seq.fromList s1) (Seq.fromList s2)
                        | s1 <- (shrinkList (const []) (toList l1)),
                          s2 <- (shrinkList (const []) (toList l2)),
                          length s1 == length s2]

naïve :: KeyFun (Seq Int)
naïve (Game p1 p2) = (p1 :|> 0) <> p2

compareKeyFun :: (Ord a, Ord b) => KeyFun a -> KeyFun b -> Game -> Property
compareKeyFun kf1 kf2 g = counterexample ("want: " <> show g1 <> ": " <> (show.score.snd) g1 <> "\n" <>
                                          "got:  " <> show g2 <> ": " <> (show.score.snd) g2) $
                          g1 == g2
  where
    g1 = play kf1 True g
    g2 = play kf2 True g

propOptFirst :: Game -> Property
propOptFirst = compareKeyFun optboth optfirst

propOptBoth :: Game -> Property
propOptBoth = compareKeyFun naïve optboth

propOptBoth' :: Game -> Property
propOptBoth' = compareKeyFun optboth optboth'

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2ex" $ testPart2For "input/day22.ex" 291,
  -- This fails with my simple/fast hand detection.
  testCase "part2 (sim642)" $ testPart2For "input/day22.sim642" 32317,
  testCase "part2" $ testPart2For "input/day22" 29177,
  testCase "part2.many" testMany,

  -- testProperty "opt first" propOptFirst,
  testProperty "opt both" propOptBoth,
  testProperty "opt both'" propOptBoth'
  ]
