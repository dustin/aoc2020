{-# LANGUAGE TypeFamilies #-}

module Day18 where

import           Control.Applicative            ((<|>))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.List                      (intercalate)
import           Data.Text                      (pack)
import           Text.Megaparsec                (between, endBy)
import           Text.Megaparsec.Char           (hspace)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Advent.AoC

data Expression = EVal Int
                | EFun (String, Int -> Int -> Int) [Expression]

type Ops = [[Operator Parser Expression]]

instance Show Expression where
  show (EVal x) = show x
  show (EFun (fn, _) exps) = intercalate fn (map inner exps)
    where inner (EVal x)             = (show) x
          inner (EFun (fn',_) exps') = "(" <> intercalate fn' (map inner exps') <> ")"

parseExpr :: [[Operator Parser Expression]] -> Parser Expression
parseExpr ops = makeExprParser term ops
  where term = parens (parseExpr ops) <|> EVal <$> L.lexeme hspace L.decimal
        parens = lexeme . between "(" ")"
        lexeme = L.lexeme hspace

op :: String -> (Int -> Int -> Int) -> Operator Parser Expression
op s f = InfixL (binify (EFun (s, f)) <$ L.symbol hspace (pack s))
  where
    binify :: ([Expression] -> Expression) -> Expression -> Expression -> Expression
    binify e a b =  e [a, b]

flat :: Ops
flat = [[op "+" (+), op "*" (*)]]

plusFirst :: Ops
plusFirst = [[op "+" (+)], [op "*" (*)]]

parseExprs :: Ops -> Parser [Expression]
parseExprs o = parseExpr o `endBy` "\n"

getInput :: Ops -> FilePath -> IO [Expression]
getInput o = parseFile (parseExprs o)

evalExpr :: Expression -> Int
evalExpr = go
  where
    go (EVal v)          = v
    go (EFun (_,f) exps) = foldr1 f (map go exps)

part1 :: [Expression] -> Int
part1 = sum . map evalExpr

part2 :: [Expression] -> Int
part2 = sum . map evalExpr
