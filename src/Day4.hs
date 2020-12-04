{-# LANGUAGE TemplateHaskell #-}

module Day4 where

import           Control.Applicative        (liftA2, (<|>))
import           Control.Lens
import           Data.Char                  (isDigit, isHexDigit, isSpace)
import           Data.Maybe                 (isJust)
import           Data.Text                  (Text, pack, unpack)
import           Text.Megaparsec            (endBy, many, option, satisfy, sepBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC

{-
byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)
-}

data Feature = BirthYear Int
             | IssueYear Int
             | Expiration Int
             | Height (Int, Text)
             | HairColor Text
             | EyeColor Color
             | PID Text
             | Country Int
             deriving Show

data Color = Amb | Blu | Brn | Gry | Grn | Hzl | Oth | InvalidColor deriving Show

makePrisms ''Feature

type Passport = [Feature]

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

isValid :: Passport -> Bool
isValid x = and [isJust (x ^? folded . _BirthYear),
                isJust (x ^? folded . _IssueYear),
                isJust (x ^? folded . _Expiration),
                isJust (x ^? folded . _Height),
                isJust (x ^? folded . _HairColor),
                isJust (x ^? folded . _EyeColor),
                isJust (x ^? folded . _PID)
                ]

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

isValid2 :: Passport -> Bool
isValid2 p = and [validBirthYear (p ^? folded . _BirthYear),
                  validIssueYear (p ^? folded . _IssueYear),
                  validExp (p ^? folded . _Expiration),
                  validHeight (p ^? folded . _Height),
                  validHairColor (p ^? folded . _HairColor),
                  validEyeColor (p ^? folded . _EyeColor),
                  validPID (p ^? folded . _PID)
                ]
  where validBirthYear = range 1920 2002
        validIssueYear = range 2010 2020
        validExp = range 2020 2030
        validHeight (Just (x, "in")) = x >= 59 && x <= 76
        validHeight (Just (x, "cm")) = x >= 150 && x <= 193
        validHeight _                = False
        validHairColor (Just "") = False
        validHairColor (Just x)
          | matches (unpack x) = True
          where matches ('#':xs) = length xs == 6 && all isHexDigit xs
                matches _        = False
        validHairColor _        = False
        validEyeColor (Just InvalidColor) = False
        validEyeColor (Just _)            = True
        validEyeColor _                   = False
        validPID (Just x) = let xs = unpack x in length xs == 9 && all isDigit xs
        validPID _        = False

        range l h (Just x) = x >= l && x <= h
        range _ _ _        = False

parseFeatures :: Parser [Feature]
parseFeatures = parseFeature `endBy` satisfy isSpace

someText :: Parser Text
someText = pack <$> many (satisfy (not . (`elem` [' ', '\n'])))

getInput :: FilePath -> IO [Passport]
getInput = parseFile (parseFeatures `sepBy` "\n")

part1 :: [Passport] -> Int
part1 = length . filter isValid

part2 :: [Passport] -> Int
part2 = length . filter isValid2
