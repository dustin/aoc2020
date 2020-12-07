module Day7 where

import           Control.Applicative        (liftA2, (<|>))
import           Control.Monad              (replicateM)
import           Data.Foldable              (fold)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, sepBy, some, try)
import           Text.Megaparsec.Char       (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

getInput :: FilePath -> IO (Map Text [(Int, Text)])
getInput = fmap fold . parseFile (parseBag `endBy` "\n")
  where parseBag = liftA2 Map.singleton (lexeme color <* lexeme "bags contain ") (contents <* ".")
        lexeme = L.lexeme space
        color = T.pack . unwords <$> replicateM 2 (lexeme (some letterChar))
        contents = [] <$ "no other bags"
                   <|> (dep `sepBy` lexeme ",")
        dep = (liftA2 (,) (lexeme L.decimal) color) <* lexeme (try "bags" <|> "bag")

part1 :: Map Text [(Int, Text)] -> Int
part1 ins = length $ filter reaches (Map.keys ms)
  where
    ms = fmap (Set.fromList . fmap snd) ins
    reaches k = Set.member "shiny gold" children || any reaches children
      where children = ms Map.! k

part2 :: Map Text [(Int, Text)] -> Int
part2 m = count "shiny gold"
  where count k = sum (fmap (\(n, k') -> n + (n * count k')) (m Map.! k))
