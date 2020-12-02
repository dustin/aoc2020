module Day2 where

import           Text.Megaparsec            (many, manyTill)
import           Text.Megaparsec.Char       (char, space)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

data Policy = Policy Int Int Char deriving Show

data PW = PW Policy String deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parsePolicy :: Parser Policy
parsePolicy = Policy <$> decimal <* "-" <*> lexeme decimal <*> L.charLiteral

parsePW :: Parser PW
parsePW = PW <$> parsePolicy <* lexeme ":" <*> lexeme (manyTill L.charLiteral (char '\n'))

getInput :: FilePath -> IO [PW]
getInput = parseFile (many parsePW)

isValid :: PW -> Bool
isValid (PW (Policy l h c) p) = cnt >= l && cnt <= h
  where cnt = length . filter (== c) $ p

isValid2 :: PW -> Bool
isValid2 (PW (Policy l h c) p) = (p !! (l - 1) == c) `xor` (p !! (h - 1) == c)
  where xor = (/=)

validateWith :: (PW -> Bool) -> [PW] -> Int
validateWith f = length . filter f

part1 :: [PW] -> Int
part1 = validateWith isValid

part2 :: [PW] -> Int
part2 = validateWith isValid2
