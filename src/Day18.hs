{-# LANGUAGE GADTs #-}

module Day18 where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData (..), rwhnf)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Semigroup                 (Sum (..))
import           Text.Megaparsec                (between, endBy)
import           Text.Megaparsec.Char           (hspace)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Advent.AoC

data Expression n where
  EVal :: !n -> Expression n
  EFun :: !(n -> n -> n) -> !(Expression n, Expression n) -> Expression n

instance NFData (Expression n) where rnf = rwhnf

type Ops n = [[Operator Parser (Expression n)]]

parseExpr :: Num n => Ops n -> Parser (Expression n)
parseExpr ops = makeExprParser term ops
  where term = parens (parseExpr ops) <|> EVal <$> lexeme L.decimal
        parens = lexeme . between "(" ")"
        lexeme = L.lexeme hspace

flat :: Num n => Ops n
flat = [[op "+" (+), op "*" (*)]]
  where
    op s f = InfixL (binify (EFun f) <$ L.symbol hspace s)
    binify e a b =  e (a, b)

plusFirst :: Num n => Ops n
plusFirst = sequenceA flat

parseExprs :: Num n => Ops n -> Parser [Expression n]
parseExprs o = parseExpr o `endBy` "\n"

evalExpr :: Num n => Expression n -> n
evalExpr (EVal v)        = v
evalExpr (EFun f (a, b)) = evalExpr a `f` evalExpr b

getInput :: Num n => Ops n -> FilePath -> IO [Expression n]
getInput o = parseFile (parseExprs o)

part1 :: [Expression (Sum Int)] -> Int
part1 = getSum . foldMap evalExpr

part2 :: [Expression (Sum Int)] -> Int
part2 = getSum . foldMap evalExpr
