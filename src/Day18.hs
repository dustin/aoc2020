{-# LANGUAGE GADTs #-}

module Day18 where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData (..), rwhnf)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Semigroup                 (Sum (..))
import           Data.Text                      (pack)
import           Text.Megaparsec                (between, endBy)
import           Text.Megaparsec.Char           (hspace)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Advent.AoC

data Expression n where
  EVal :: !n -> Expression n
  EFun :: (String, n -> n -> n) -> !(Expression n, Expression n) -> Expression n

instance NFData (Expression n) where rnf = rwhnf

-- Fancy Show instance of expressions that turn them into expressions
-- as would be evaluated (or can be parsed again).
instance Show n => Show (Expression n) where
  show (EVal x) = show x
  show e@(EFun _ _) = sx e
    where inner (EVal x)      = show x
          inner e'@(EFun _ _) = "(" <> sx e' <> ")"
          sx (EVal x)              = show x
          sx (EFun (fn,_) (ea,eb)) = inner ea <> fn <> inner eb

type Ops n = [[Operator Parser (Expression n)]]

parseExpr :: Num n => Ops n -> Parser (Expression n)
parseExpr ops = makeExprParser term ops
  where term = parens (parseExpr ops) <|> EVal <$> lexeme L.decimal
        parens = lexeme . between "(" ")"
        lexeme = L.lexeme hspace

flat :: Num n => Ops n
flat = [[op "+" (+), op "*" (*)]]
  where
    op s f = InfixL (binify (EFun (s, f)) <$ L.symbol hspace (pack s))
    binify e a b =  e (a, b)

plusFirst :: Num n => Ops n
plusFirst = sequenceA flat

parseExprs :: Num n => Ops n -> Parser [Expression n]
parseExprs o = parseExpr o `endBy` "\n"

evalExpr :: Num n => Expression n -> n
evalExpr (EVal v)             = v
evalExpr (EFun (_,f) (ea,eb)) = evalExpr ea `f` evalExpr eb

getInput :: Num n => Ops n -> FilePath -> IO [Expression n]
getInput o = parseFile (parseExprs o)

part1 :: [Expression (Sum Int)] -> Int
part1 = getSum . foldMap evalExpr

part2 :: [Expression (Sum Int)] -> Int
part2 = getSum . foldMap evalExpr
