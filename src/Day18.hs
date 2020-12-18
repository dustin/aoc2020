module Day18 where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData (..), rwhnf)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.List                      (intercalate)
import           Data.Semigroup                 (Sum (..))
import           Data.Text                      (pack)
import           Text.Megaparsec                (between, endBy)
import           Text.Megaparsec.Char           (hspace)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Advent.AoC

data Expression n = EVal n
                  | EFun (String, n -> n -> n) [Expression n]

instance NFData (Expression n) where rnf = rwhnf

type Ops n = [[Operator Parser (Expression n)]]

instance Show n => Show (Expression n) where
  show (EVal x) = show x
  show (EFun (fn, _) exps) = intercalate fn (map inner exps)
    where inner (EVal x)             = (show) x
          inner (EFun (fn',_) exps') = "(" <> intercalate fn' (map inner exps') <> ")"

parseExpr :: Num n => [[Operator Parser (Expression n)]] -> Parser (Expression n)
parseExpr ops = makeExprParser term ops
  where term = parens (parseExpr ops) <|> EVal <$> L.lexeme hspace L.decimal
        parens = lexeme . between "(" ")"
        lexeme = L.lexeme hspace

op :: String -> (n -> n -> n) -> Operator Parser (Expression n)
op s f = InfixL (binify (EFun (s, f)) <$ L.symbol hspace (pack s))
  where
    binify :: ([Expression n] -> Expression n) -> Expression n -> Expression n -> Expression n
    binify e a b =  e [a, b]

flat :: Num n => Ops n
flat = [[op "+" (+), op "*" (*)]]

plusFirst :: Num n => Ops n
plusFirst = [[op "+" (+)], [op "*" (*)]]

parseExprs :: Num n => Ops n -> Parser [Expression n]
parseExprs o = parseExpr o `endBy` "\n"

getInput :: Num n => Ops n -> FilePath -> IO [Expression n]
getInput o = parseFile (parseExprs o)

evalExpr :: Num n => Expression n -> n
evalExpr = go
  where
    go (EVal v)          = v
    go (EFun (_,f) exps) = foldr1 f (map go exps)

part1 :: [Expression (Sum Int)] -> Int
part1 = getSum . foldMap evalExpr

part2 :: [Expression (Sum Int)] -> Int
part2 = getSum . foldMap evalExpr
