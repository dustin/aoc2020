module Day23 where

import           Data.Char     (intToDigit)
import           Data.Foldable (toList)
import           Data.Maybe    (fromJust)
import           Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq

import           Advent.AoC

cups :: Seq Int
cups = Seq.fromList [1,3,5,4,6,8,7,2,9]

play :: Int -> Seq Int -> Seq Int
play maxn !(dest1 :<| xs) = (chop1 <> next3 <> chop2) |> dest1
  where (next3, rest) = Seq.splitAt 3 xs
        dest = guessSeq (dest1-1)
        destPos = fromJust (Seq.elemIndexL dest rest) + 1
        (chop1, chop2) = Seq.splitAt destPos rest
        guessSeq 0 = guessSeq maxn
        guessSeq n
          | elem n next3 = guessSeq (n - 1)
          | otherwise = n

label :: Seq Int -> String
label cs = fmap intToDigit . toList $ wrapped
  where pos = fromJust (Seq.elemIndexL 1 cs) + 1
        pos' = if pos == length cs - 1 then 0 else pos
        (a :|> _, b) = Seq.splitAt pos' cs
        wrapped = b <> a

part1 :: String
part1 = label (ntimes 100 (play 9) cups)

cups2 :: Seq Int
cups2 = cups <> Seq.fromList [10..1000000]

part2 :: Int
part2 = a * b
  where
    played = ntimes 10000000 (play 1000000) cups2
    pos = fromJust (Seq.elemIndexL 1 played) + 1
    a = Seq.index played pos
    b = Seq.index played (pos+1)
