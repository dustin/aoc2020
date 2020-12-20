{-# LANGUAGE DeriveFunctor #-}

module Day20 where

import           Control.Applicative        (liftA2)
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.List                  (transpose)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Text.Megaparsec            (many, satisfy, some)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

type Fragment = [String]

data Tile a = Tile {
  _tileNum    :: Int
  , _tileFrag :: a
  } deriving (Ord, Eq, Show, Functor)

instance NFData a => NFData (Tile a) where rnf = rwhnf

parseTile :: Parser (Tile Fragment)
parseTile = Tile <$> tileNum <*> aMap
  where
    tileNum = "Tile " *> L.decimal <* ":\n"
    aMap = filter (not.null) . lines <$> many (satisfy (`elem` ['.', '#', '\n']))

getInput :: FilePath -> IO [Tile Fragment]
getInput = parseFile (some parseTile)

sideMap :: [Tile Fragment] -> Map MapEdge (Set Int)
sideMap inp = Map.unionsWith (<>) (edgesFor <$> te)
  where
    edgesFor :: Tile MapEdges -> Map MapEdge (Set Int)
    edgesFor t@Tile{..} = Map.fromListWith (<>) [(e, Set.singleton _tileNum) | a <- arrangements t, e <- edga a]
    edga (Tile _ (a,b,c,d)) = [a,b,c,d]
    te = (fmap.fmap) sides inp
    arrangements t = (Set.toList . Set.fromList) $ (<$> t) <$> (liftA2 (.) flips rotations)
      where flips = [flipEdgesX, flipEdgesY, flipEdgesX . flipEdgesY, flipEdgesY . flipEdgesX]
            rotations = (\n -> ntimes n rotateEdges) <$> [0..3]

type MapEdge = String

-- left, top, right, bottom
type MapEdges = (MapEdge, MapEdge, MapEdge, MapEdge)

sides :: Fragment -> MapEdges
sides f = (head <$> f,
           head f,
           last <$> f,
           last f)

flipFragX :: Fragment -> Fragment
flipFragX = fmap reverse

flipFragY :: Fragment -> Fragment
flipFragY = reverse

rotateFrag :: Fragment -> Fragment
rotateFrag = reverse . transpose

rotateEdges :: MapEdges -> MapEdges
rotateEdges (a,b,c,d) = (reverse b, c, reverse d, a)

flipEdgesX :: MapEdges -> MapEdges
flipEdgesX (a,b,c,d) = (c, reverse b, a, reverse d)

flipEdgesY :: MapEdges -> MapEdges
flipEdgesY (a,b,c,d) = (reverse a, d, reverse c, b)

fitsL :: MapEdges -> MapEdges -> Bool
fitsL (_, _, l, _) (r, _, _, _) = l == r

fitsT :: MapEdges -> MapEdges -> Bool
fitsT (_, _, _, t) (_, b, _, _) = t == b

ntimes :: Int -> (a -> a) -> a -> a
ntimes n f a = iterate f a !! n

flipMap :: Ord b => Map a b -> Map b [a]
flipMap m = Map.fromListWith (<>) [(v,[k]) | (k,v) <- Map.assocs m]

-- Map of all of the tiles that belong on the edges (i.e., have at
-- least one side with no common border) to the True if a corner.
edges :: [Tile Fragment] -> Map Int Bool
edges = fmap (== 4)
        . Map.fromListWith (+)
        . foldMap (fmap (,1::Int) . Set.toList)
        . Map.filter (\x -> Set.size x == 1) . sideMap

corners :: [Tile Fragment] -> [Int]
corners = (Map.! True) . flipMap . edges

part1 :: [Tile Fragment] -> Int
part1 = product . corners

part2 :: [Tile Fragment] -> Int
part2 = const 0
