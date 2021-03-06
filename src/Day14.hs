module Day14 where

import           Control.Applicative        (liftA2, (<|>))
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.Bits                  (complement, (.&.), (.|.))
import           Data.Foldable              (foldl')
import qualified Data.IntMap.Strict         as IntMap
import           Text.Megaparsec            (endBy, satisfy, some)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC

type Program = [Manipulation]

type BitMask = (Int, Int)

data Manipulation = Mask !BitMask ![BitMask] -- zero mask, one mask, xmasks
                  | Mem !Int !Int deriving Show

instance NFData Manipulation where rnf = rwhnf

getInput :: FilePath -> IO Program
getInput = parseFile parseProgram

parseProgram :: Parser Program
parseProgram = (parseMem <|> parseMask) `endBy` "\n"

  where
    parseMem = liftA2 Mem ("mem[" *> decimal <* "] = ") decimal

    parseMask = do
      rawMask <- "mask = " *> some (satisfy (`elem` ['X', '0', '1']))
      let (z,o) = masks rawMask
          xes = masks <$> traverse (\case 'X' -> "01"; '0' -> "."; x -> [x]) rawMask
      pure $ Mask (z, o) xes

        where
          masks r = (complement (mkmask '0' r), mkmask '1' r)
          mkmask c = foldl' (\o' x -> o' * 2 + if x == c then 1 else 0) 0

applyMask :: BitMask -> Int -> Int
applyMask !(!z, !o) w = w .&. z .|. o

part1 :: Program -> Int
part1 = sum . run mempty (0, 0)
  where
    run s _ []              = s
    run s _ (Mask m _:xs)   = run s m xs
    run s mask (Mem i w:xs) = run (IntMap.insert i (applyMask mask w) s) mask xs

part2 :: Program -> Int
part2 = sum . run mempty mempty
  where
    run s _ []               = s
    run s _ (Mask _ ms:xs)   = run s ms xs
    run s masks (Mem i w:xs) = run (IntMap.fromList [(applyMask m i, w) | m <- masks] <> s) masks xs
