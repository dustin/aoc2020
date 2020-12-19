module Day19 where

import           Control.Applicative          (liftA2, (<|>))
import           Control.DeepSeq              (NFData (..), rwhnf)
import           Data.Functor                 (($>))
import           Data.List                    (foldl1')
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text, pack, unpack)
import           Text.Megaparsec              (anySingle, endBy, many, option, satisfy, some)
import           Text.Megaparsec.Char         (hspace, space)
import qualified Text.Megaparsec.Char.Lexer   as L
import qualified Text.ParserCombinators.ReadP as R

import           Advent.AoC
import           Advent.Search

data Rule = Rule [Int]
          | RuleOr [Int] [Int]
          | RuleLit Char
  deriving Show

instance NFData Rule where rnf = rwhnf

type RuleMap = Map Int Rule

parseRule :: Parser (Int, Rule)
parseRule = liftA2 (,) (L.decimal <* lexeme ":") (parseChar <|> parseSeq)
  where
    parseChar = RuleLit <$> ("\"" *> anySingle <* "\"")
    parseSeq = do
      rs <- nums
      alt <- option [] (lexeme "|" *> nums)
      pure $ case alt of
               [] -> Rule rs
               oz -> RuleOr rs oz

    nums = many (lexeme L.decimal)
    lexeme = L.lexeme hspace

parseInput :: Parser (RuleMap, [Text])
parseInput = do
  rs <- Map.fromList <$> L.lexeme space (parseRule `endBy` "\n")
  ss <- (some (satisfy (`elem` ['a', 'b'])) `endBy` "\n")
  pure (rs, pack <$> ss)

mkParser :: RuleMap -> Rule -> R.ReadP ()
mkParser rm = go
  where
    go (RuleLit c)  = R.satisfy (== c) $> ()
    go (RuleOr l r) = p1 l <|> p1 r
    go (Rule l)     = p1 l
    p1 ls = foldl1' (*>) (go . (rm Map.!) <$> ls)

getInput :: FilePath -> IO (RuleMap, [Text])
getInput = parseFile parseInput

part1 :: (RuleMap, [Text]) -> Int
part1 (rm, ins) = countIf rp ins
  where
    p' = mkParser rm (rm Map.! 0) *> R.eof
    rp x = (not.null) $ R.readP_to_S p' (unpack x)

part2 :: (RuleMap, [Text]) -> Int
part2 (rm, ins) = part1 (rm', ins)
  where
    rm' = (Map.fromList [(8, RuleOr [42] [42, 8]),
                         (11, RuleOr [42, 31] [42, 11, 31])]) <> rm

