{-# LANGUAGE DeriveGeneric      #-}
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
  _shipDir    :: Dir
  , _shipPos  :: Point
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

doit :: (Action -> Ship -> Ship) -> [Action] -> Int
doit f = mdist2 (0,0) . _shipPos . foldl' (flip f) (Ship E (0,0) (10,1))

part1 :: [Action] -> Int
part1 = doit apply
  where
    apply (MoveDir dir amt) = shipPos %~ fwdBy amt dir
    apply (Forward amt)     = \s@Ship{_shipDir} -> s & shipPos %~ fwdBy amt _shipDir
    apply (TurnLeft n)      = shipDir %~ ntimes n pred'
    apply (TurnRight n)     = shipDir %~ ntimes n succ'

part2 :: [Action] -> Int
part2 = doit apply
  where
    apply (MoveDir dir amt) = wayPoint %~ fwdBy amt dir
    apply (Forward amt)     = \s@Ship{_wayPoint} -> s & shipPos  %~ (addPoint . mulPoint (amt, amt)) _wayPoint
    apply (TurnLeft n)      = wayPoint %~ ntimes n rotateLeft
    apply (TurnRight n)     = wayPoint %~ ntimes n rotateRight

    rotateLeft (x,y) = (-y, x)
    rotateRight (x,y) = (y, -x)
