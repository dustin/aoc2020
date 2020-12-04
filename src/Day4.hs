{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Day4 where

import           Control.Applicative        (liftA2, (<|>))
import           Control.Lens
import           Data.Char                  (isDigit, isHexDigit, isSpace)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, many, option, satisfy, sepBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC
import           Advent.Search

data Color = Amb | Blu | Brn | Gry | Grn | Hzl | Oth | InvalidColor deriving (Show, Eq)

data Feature = BirthYear Int
             | IssueYear Int
             | Expiration Int
             | Height (Int, Text)
             | HairColor Text
             | EyeColor Color
             | PID Text
             | Country Int
             deriving Show

makePrisms ''Feature

data Passport = Passport {
  _birthYear    :: Int
  , _issueYear  :: Int
  , _expiration :: Int
  , _height     :: (Int, Text)
  , _hairColor  :: Text
  , _eyeColor   :: Color
  , _pid        :: Text
  } deriving Show

parsePassport :: Parser (Maybe Passport)
parsePassport = parseFeatures >>= \fs ->
  pure $ Passport <$> (fs ^? folded . _BirthYear)
    <*> (fs ^? folded . _IssueYear)
    <*> (fs ^? folded . _Expiration)
    <*> (fs ^? folded . _Height)
    <*> (fs ^? folded . _HairColor)
    <*> (fs ^? folded . _EyeColor)
    <*> (fs ^? folded . _PID)

parseFeature :: Parser Feature
parseFeature = BirthYear <$> ("byr:" *> decimal)
               <|> IssueYear <$> ("iyr:" *> decimal)
               <|> Expiration <$> ("eyr:" *> decimal)
               <|> Height <$> liftA2 (,) ("hgt:" *> decimal) (option "" someText)
               <|> PID <$> ("pid:" *> someText)
               <|> Country <$> ("cid:" *> decimal)
               <|> HairColor <$> ("hcl:" *> someText)
               <|> EyeColor <$> ("ecl:" *> eyeColor)

  where
    eyeColor :: Parser Color
    eyeColor = Amb <$ "amb"
               <|> Blu <$ "blu"
               <|> Brn <$ "brn"
               <|> Gry <$ "gry"
               <|> Grn <$ "grn"
               <|> Hzl <$ "hzl"
               <|> Oth <$ "oth"
               <|> InvalidColor <$ someText

parseFeatures :: Parser [Feature]
parseFeatures = parseFeature `endBy` satisfy isSpace

someText :: Parser Text
someText = T.pack <$> many (satisfy (not . (`elem` [' ', '\n'])))

getInput :: FilePath -> IO [Passport]
getInput = fmap catMaybes . parseFile (parsePassport `sepBy` "\n")

part1 :: [Passport] -> Int
part1 = length

{-
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
-}

part2 :: [Passport] -> Int
part2 = countIf isValid
  where
    isValid Passport{..} = and [range 1920 2002 _birthYear,
                                range 2010 2020 _issueYear,
                                range 2020 2030 _expiration,
                                validHeight _height,
                                isHexColor _hairColor,
                                _eyeColor /= InvalidColor,
                                validPID _pid]
    validHeight (x, "in") = range 59 76 x
    validHeight (x, "cm") = range 150 193 x
    validHeight _         = False
    validPID x = T.length x == 9 && T.all isDigit x

    range l h x = x >= l && x <= h
    isHexColor (T.stripPrefix "#" -> Just xs) = T.length xs == 6 && T.all isHexDigit xs
    isHexColor _                              = False
