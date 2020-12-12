{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# Options_GHC -Wno-orphans #-}

module Day12 where

import           Control.Applicative        ((<|>))
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Data.Foldable              (foldl')
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC
import           Advent.TwoD

data Action = MoveDir Dir Int
            | TurnLeft Int
            | TurnRight Int
            | Forward Int
            deriving (Show)

instance NFData Dir where rnf = rwhnf

instance NFData Action where rnf = rwhnf

data Ship = Ship {
  shipDir    :: Dir
  , shipPos  :: Point
  , wayPoint :: Point
  } deriving Show

parseAction :: Parser Action
parseAction = parseDir <|> parseLeft <|> parseRight <|> parseForward
  where
    parseDir = MoveDir <$> parseD <*> decimal
    parseD = N <$ "N" <|> E <$ "E" <|> S <$ "S" <|> W <$ "W"
    parseLeft = TurnLeft <$> ("L" *> decimal)
    parseRight = TurnRight <$> ("R" *> decimal)
    parseForward = Forward <$> ("F" *> decimal)

getInput :: FilePath -> IO [Action]
getInput = parseFile (parseAction `endBy` "\n")

ntimes :: Int -> (a -> a) -> a -> a
ntimes n f a = iterate f a !! n

doit :: (Ship -> Action -> Ship) -> [Action] -> Int
doit f = mdist2 (0,0) . shipPos . foldl' f (Ship E (0,0) (10,1))

part1 :: [Action] -> Int
part1 = doit apply
  where
    apply ship@Ship{..} (MoveDir dir amt) = ship{shipPos = fwdBy amt dir shipPos}
    apply ship@Ship{..} (Forward amt)     = ship{shipPos = fwdBy amt shipDir shipPos}
    apply ship@Ship{..} (TurnLeft n)      = ship{shipDir = (ntimes (n `div` 90) pred') shipDir}
    apply ship@Ship{..} (TurnRight n)     = ship{shipDir = (ntimes (n `div` 90) succ') shipDir}

part2 :: [Action] -> Int
part2 = doit apply
  where
    apply ship@Ship{..} (MoveDir dir amt) = ship{wayPoint = fwdBy amt dir wayPoint}
    apply ship@Ship{..} (Forward amt)     = ship{shipPos = addPoint (mulPoint (amt, amt) wayPoint) shipPos}
    apply ship@Ship{..} (TurnLeft n)      = ship{wayPoint = (ntimes (n `div` 90) rotateLeft) wayPoint}
    apply ship@Ship{..} (TurnRight n)     = ship{wayPoint = (ntimes (n `div` 90) rotateRight) wayPoint}

    rotateLeft (x,y) = (-y, x)
    rotateRight (x,y) = (y, -x)
