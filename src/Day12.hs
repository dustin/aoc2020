{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# Options_GHC -Wno-orphans #-}

module Day12 where

import           Control.Applicative        ((<|>))
import           Control.DeepSeq            (NFData (..), rwhnf)
import           Control.Lens
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
  _shipPos    :: Point
  , _wayPoint :: Point
  } deriving Show

makeLenses ''Ship

parseAction :: Parser Action
parseAction = parseDir <|> parseLeft <|> parseRight <|> parseForward
  where
    parseDir = MoveDir <$> parseD <*> decimal
    parseD = N <$ "N" <|> E <$ "E" <|> S <$ "S" <|> W <$ "W"
    parseLeft = TurnLeft <$> ("L" *> fmap (`div` 90) decimal)
    parseRight = TurnRight <$> ("R" *> fmap (`div` 90) decimal)
    parseForward = Forward <$> ("F" *> decimal)

parseActions :: Parser [Action]
parseActions = parseAction `endBy` "\n"

getInput :: FilePath -> IO [Action]
getInput = parseFile parseActions

ntimes :: Int -> (a -> a) -> a -> a
ntimes n f a = iterate f a !! n

doit :: Lens' Ship Point -> Ship -> [Action] -> Int
doit ml inship = mdist2 (0,0) . _shipPos . foldl' (flip apply) inship
  where
    apply (MoveDir dir amt) = ml %~ fwdBy amt dir
    apply (Forward amt)     = \s@Ship{_wayPoint} -> s & shipPos  %~ (addPoint . mulPoint (amt, amt)) _wayPoint
    apply (TurnLeft n)      = wayPoint %~ ntimes n rotateLeft
    apply (TurnRight n)     = wayPoint %~ ntimes n rotateRight

    rotateLeft (x,y) = (-y, x)
    rotateRight (x,y) = (y, -x)

part1 :: [Action] -> Int
part1 = doit shipPos (Ship (0,0) (1,0))

part2 :: [Action] -> Int
part2 = doit wayPoint (Ship (0,0) (10,1))
