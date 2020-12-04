module Day2 where

import           Text.Megaparsec            (many, manyTill)
import           Text.Megaparsec.Char       (char, space)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC
import           Advent.Search

data Policy = Policy Int Int Char deriving Show

data PW = PW Policy String deriving Show

parsePW :: Parser PW
parsePW = PW <$> parsePolicy <* lexeme ":" <*> lexeme (manyTill L.charLiteral (char '\n'))
  where parsePolicy = Policy <$> decimal <* "-" <*> lexeme decimal <*> L.charLiteral
        lexeme = L.lexeme space

getInput :: FilePath -> IO [PW]
getInput = parseFile (many parsePW)

part1 :: [PW] -> Int
part1 = countIf valid
  where valid (PW (Policy l h c) p) = cnt >= l && cnt <= h
          where cnt = countIf (== c) p

part2 :: [PW] -> Int
part2 = countIf valid
  where valid (PW (Policy l h c) p) = (p !! (l - 1) == c) `xor` (p !! (h - 1) == c)
        xor = (/=)
