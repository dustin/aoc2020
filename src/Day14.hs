{-# LANGUAGE LambdaCase #-}

module Day14 where

import           Control.Applicative        (liftA2, (<|>))
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.Bits                  (complement, (.&.), (.|.))
import           Data.Foldable              (foldl')
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Word                  (Word64)
import           Text.Megaparsec            (endBy, satisfy, some)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC

type Program = [Manipulation]

type BitMask = (Word64, Word64)

data Manipulation = Mask BitMask [BitMask] -- zero mask, one mask, xmasks
                  | Mem Word64 Word64 deriving Show

instance NFData Manipulation where rnf = rwhnf

getInput :: FilePath -> IO Program
getInput = parseFile parseProgram

parseProgram :: Parser Program
parseProgram = seg `endBy` "\n"
  where
    seg = parseMem <|> parseMask

parseMem :: Parser Manipulation
parseMem = liftA2 Mem ("mem[" *> decimal <* "] = ") decimal

parseMask :: Parser Manipulation
parseMask = do
  _ <- "mask = "
  rawMask <- some (satisfy (`elem` ['X', '0', '1']))
  let (z,o) = masks rawMask
      xes = masks <$> traverse (\case 'X' -> "01"; x -> [x]) (map (\case '0' -> 'N'; x -> x) rawMask)
  pure $ Mask (z, o) xes

  where
    masks r = (zmask r, omask r)
    zmask = foldl' (\o' x -> o' * 2 + if x == '0' then 1 else 0) 0
    omask = foldl' (\o' x -> o' * 2 + if x == '1' then 1 else 0) 0

applyMask :: BitMask -> Word64 -> Word64
applyMask (z, o) w = w .&. complement z .|. o

part1 :: Program -> Word64
part1 = sum . run mempty (0, 0)
  where
    run :: Map Word64 Word64 -> BitMask -> [Manipulation] -> Map Word64 Word64
    run s _ []              = s
    run s _ (Mask m _:xs)   = run s m xs
    run s mask (Mem i w:xs) = run (Map.insert i (applyMask mask w) s) mask xs

part2 :: Program -> Word64
part2 = sum . run mempty mempty
  where
    run :: Map Word64 Word64 -> [BitMask] -> [Manipulation] -> Map Word64 Word64
    run s _ []               = s
    run s _ (Mask _ ms:xs)   = run s ms xs
    run s masks (Mem i w:xs) = run (foldr (\x o -> Map.insert (applyMask x i) w o) s masks) masks xs
