{-# LANGUAGE DeriveFunctor #-}

module Day20 where

import           Control.Applicative        (liftA2)
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.Foldable              (fold)
import           Data.List                  (transpose)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Text.Megaparsec            (many, satisfy, some)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC
import           Advent.Search
import           Advent.TwoD
import           Advent.Vis

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

-- Find possible orientations of a Fragment based on a predicate
orient :: (Fragment -> Bool) -> Fragment -> [Fragment]
orient p = filter p . ars
  where
    ars f = Set.toList . Set.fromList $ (($f) <$> txs)
      where
        txs = liftA2 (.) flips rotations
        flips = [flipFragX, flipFragY, flipFragX . flipFragY, flipFragY . flipFragX]
        rotations = (\n -> ntimes n rotateFrag) <$> [0..3]

rotateEdges :: MapEdges -> MapEdges
rotateEdges (a,b,c,d) = (reverse b, c, reverse d, a)

flipEdgesX :: MapEdges -> MapEdges
flipEdgesX (a,b,c,d) = (c, reverse b, a, reverse d)

flipEdgesY :: MapEdges -> MapEdges
flipEdgesY (a,b,c,d) = (reverse a, d, reverse c, b)

fitsFL :: Fragment -> Fragment -> Bool
fitsFL a b = fitsL (sides a) (sides b)
  where fitsL (_, _, l, _) (r, _, _, _) = l == r

fitsFT :: Fragment -> Fragment -> Bool
fitsFT a b = fitsT (sides a) (sides b)
  where fitsT (_, _, _, t) (_, b', _, _) = t == b'

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

seaMonster :: Fragment
seaMonster =[
  "                  #",
  "#    ##    ##    ###",
  " #  #  #  #  #  #"]

identifyMonsters :: Fragment -> ([Point], String)
identifyMonsters fs = (identified, drawString replaceAll (replaceAll Map.!))
  where
    replaceAll = foldr (\x o -> Map.union (replace x) o) fraggrid identified
    replace off = Map.fromList [(addPoint off k, 'O') | k <- replacer]
    replacer = catMaybes . zipWith2D (\x y -> \case '#' -> Just (x,y); _ -> Nothing) [0..] [0..] $ seaMonster
    identified = [(x,y) | x <- [0 .. maxx], y <- [0 .. maxy], matchAt fraggrid (x,y) monstermatch]
    (_,(maxx,maxy)) = bounds2d fraggrid

    fraggrid = gridify id fs
    monstermatch = catMaybes . zipWith2D (\x y -> \case '#' -> Just ((x,y), (== '#')); _ -> Nothing) [0..] [0..] $ seaMonster
    matchAt m off = foldr (\(k,f) o -> f (Map.findWithDefault '.' (addPoint off k) m) && o) True
    gridify f = Map.fromList . zipWith2D (\x y a -> ((x,y), f a)) [0..] [0..]

properOrder :: [Tile Fragment] -> [[Tile Fragment]]
properOrder inp = order
  where
    fragMap = Map.fromList [(_tileNum, _tileFrag) | Tile{..} <- inp]
    allSides = sideMap inp
    c1 = (\x -> Tile x (fragMap Map.! x)) . head . Map.keys . Map.filter id . edges $ inp
    neighborCount s = length (allSides Map.! s)
    neighbors Tile{..} = fit . ftup4 (Set.toList . Set.delete _tileNum . (allSides Map.!)) . sides $ _tileFrag
      where fit (_,_,[r],[b]) = (nbs fitsFL r, nbs fitsFT b)
            fit (_,_,[r],[])  = (nbs fitsFL r, [])
            fit x             = error ("oh no: " <> show x)

            -- neighbors oriented to fit a particular way
            nbs ff n = [Tile n x | x <- orient (ff _tileFrag) (fragMap Map.! n)]

    rightOf = head . fst . neighbors
    below = head . snd . neighbors

    gridSize = ceiling . sqrt . fromIntegral . length $ inp

    order = fmap (take gridSize . iterate rightOf) (take gridSize (iterate below firstCorner))

    firstCorner = (head . orient suchThat) <$> c1
      where suchThat c = neighborCount l == 1 && neighborCount t == 1
              where (l,t,_,_) = sides c

fullImage :: [[Tile Fragment]] -> Fragment
fullImage order = foldMap (fmap fold . transpose) frags
  where
    erase1 l = tail (reverse (tail (reverse l)))
    eraseBorder = fmap erase1 . erase1
    unbordered = (fmap.fmap.fmap) eraseBorder order
    frags = (fmap . fmap) _tileFrag $ unbordered

part2 :: [Tile Fragment] -> Int
part2 = countIf (== '#') . snd . identifyMonsters . head . orient (not . null . fst . identifyMonsters) . fullImage . properOrder
