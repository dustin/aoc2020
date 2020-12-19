{-# LANGUAGE GADTs #-}

module Day18 where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData (..), rwhnf)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Foldable                  (fold)
import           Data.Semigroup                 (Sum (..))
import           Text.Megaparsec                (between, endBy)
import           Text.Megaparsec.Char           (hspace)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Advent.AoC

data Expression n where
  EVal :: !n -> Expression n
  EFun :: !(n -> n -> n) -> !(Expression n) -> !(Expression n) -> Expression n

instance Semigroup n => Semigroup (Expression n) where
  a <> b = EFun (<>) a b

instance Monoid n => Monoid (Expression n) where
  mempty = EVal mempty

instance Foldable Expression where
  foldMap f (EVal x) = f x
  foldMap f e@(EFun _ _ _) = f (eval e)
    where eval (EVal x)     = x
          eval (EFun o a b) = eval a `o` eval b

instance NFData (Expression n) where rnf = rwhnf

type Ops n = [[Operator Parser (Expression n)]]

parseExpr :: Num n => Ops n -> Parser (Expression n)
parseExpr ops = makeExprParser term ops
  where term = L.lexeme hspace (between "(" ")" (parseExpr ops))
               <|> EVal <$> L.lexeme hspace L.decimal

flat :: Num n => Ops n
flat = (fmap.fmap) (\(s,f) -> InfixL (EFun f <$ L.symbol hspace s)) [[("+",(+)), ("*", (*))]]

plusFirst :: Num n => Ops n
plusFirst = sequenceA flat

parseExprs :: Num n => Ops n -> Parser [Expression n]
parseExprs o = parseExpr o `endBy` "\n"

getInput :: Num n => Ops n -> FilePath -> IO [Expression n]
getInput o = parseFile (parseExprs o)

ans :: (Num n, Monoid n) => [Expression n] -> n
ans = fold . fold

part1 :: [Expression (Sum Int)] -> Sum Int
part1 = ans

part2 :: [Expression (Sum Int)] -> Sum Int
part2 = ans
