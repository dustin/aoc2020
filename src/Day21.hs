module Day21 where

import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.Char                  (isLetter)
import           Data.List                  (sortOn)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, satisfy, sepBy, some)
import           Text.Megaparsec.Char       (hspace)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

data Item = Item {
  _ingredients :: Set Text
  , _allergens :: Set Text
  } deriving Show

instance NFData Item where rnf = rwhnf

parseItem :: Parser Item
parseItem = Item <$> is <*> as
  where
    is = Set.fromList <$> some (lexeme word) <* "(contains "
    as = Set.fromList <$> (word `sepBy` lexeme ",") <* ")"
    lexeme = L.lexeme hspace
    word = T.pack <$> some (satisfy isLetter)

parseItems :: Parser [Item]
parseItems = parseItem `endBy` "\n"

getInput :: FilePath -> IO [Item]
getInput = parseFile parseItems

allergens :: [Item] -> Map Text (Set Text)
allergens inp = Map.mapWithKey check im
  where
    check k = Set.filter (\a -> allIn k (am Map.! a))
    allIn k = all (k `Set.member`)
    am = Map.fromListWith (<>) [(a,[_ingredients])
                               | Item{..} <- inp
                               , a <- Set.toList _allergens]
    im = Map.fromListWith (<>) [(i,Set.singleton a)
                               | Item{..} <- inp
                               , i <- Set.toList _ingredients
                               , a <- Set.toList _allergens]

part1 :: [Item] -> Int
part1 inp = counts inp
  where
    counts = sum . fmap (length . (`Set.intersection` noAlls) . _ingredients)
    noAlls = Map.keysSet . Map.filter null . allergens $ inp

part2 :: [Item] -> Text
part2 = T.intercalate "," . fmap fst . sortOn snd . valids . candidates
  where
    candidates = srt . Map.assocs . Map.filter (not.null) . allergens
    valids ((i,s):xs)
      | Set.size s == 1 = (i, one s) : valids (srt (fmap (one s `Set.delete`) <$> xs))
    valids _ = []
    srt = sortOn (length . snd)
    one = head . Set.toList
