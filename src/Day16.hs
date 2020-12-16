module Day16 where

import           Control.Applicative        (liftA2)
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.Char                  (isLetter)
import           Data.Foldable              (fold)
import           Data.IntSet                (IntSet)
import qualified Data.IntSet                as IntSet
import           Data.List                  (sort, transpose)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, manyTill, satisfy, sepBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC

data Input = Input {
  _fields   :: [Field]
  , _ticket :: [Int]
  , _nearby :: [[Int]]
  } deriving Show

instance NFData Input where rnf = rwhnf

data Field = Field {
  _fieldName   :: Text
  , _fieldNums :: IntSet
  }

instance NFData Field where rnf = rwhnf

instance Eq Field where
  (Field a _) == (Field b _) = a == b

instance Ord Field where
  (Field a _) <= (Field b _) = a <= b

instance Show Field where
  show (Field t r) = "Field " <> show t <> " (" <> show (IntSet.size r) <> " nums)"

parseField :: Parser Field
parseField = Field . T.pack
             <$> (manyTill (satisfy (\c -> c == ' ' || isLetter c)) ": ")
             <*> (foldMap IntSet.fromList <$> range `sepBy` " or ")

  where range = liftA2 enumFromTo (decimal <* "-") decimal

parseInput :: Parser Input
parseInput = do
  _fields <- parseField `endBy` "\n"
  _ticket <- "\nyour ticket:\n" *> aTicket <* "\n\n"
  _nearby <- "nearby tickets:\n" *> aTicket `endBy` "\n"
  pure $ Input{..}

  where aTicket = decimal `sepBy` ","

getInput :: FilePath -> IO Input
getInput = parseFile parseInput

part1 :: Input -> Int
part1 Input{..} = sum . filter (`IntSet.notMember` valids) $ fold _nearby
  where valids = foldMap _fieldNums _fields

validArrangements :: Input -> [Field]
validArrangements Input{..} = ffilter

  where
    valids = foldMap _fieldNums _fields
    fieldsets = fmap (\tt -> filter (\f -> tt `IntSet.isSubsetOf` _fieldNums f) _fields) ttickets
    ttickets = IntSet.fromList . filter (`IntSet.member` valids) <$> transpose _nearby
    ffilter = fmap snd . sort . go mempty mempty $ (sort $ zip3 (length <$> fieldsets) [0 :: Int ..] fieldsets)
      where
        go _ r []             = r
        go s r ((_,pos,x):xs) = let [n] = filter (`Set.notMember` s) x in go (Set.insert n s) ((pos,n):r) xs

part2 :: Input -> Int
part2 inp@Input{..} = product (snd <$> deps)
  where deps = filter (("departure" `T.isPrefixOf`) . _fieldName . fst) named
        named = zip (validArrangements inp) _ticket
