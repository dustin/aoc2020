module Day8 where

import           Control.Applicative        ((<|>))
import           Data.Either                (fromLeft, rights)
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import           Text.Megaparsec            (some)
import           Text.Megaparsec.Char       (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

data Operation = NOOP | ACC | JMP deriving Show

data Instruction = Instruction Operation Int deriving Show

type Program = V.Vector Instruction

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseInstr :: Parser Instruction
parseInstr = Instruction <$> lexeme op <*> L.signed space L.decimal <* "\n"
  where
    op = NOOP <$ "nop"
         <|> ACC <$ "acc"
         <|> JMP <$ "jmp"

getInput :: FilePath -> IO Program
getInput = fmap V.fromList . parseFile (some parseInstr)

evalStep :: Program -> Int -> Int -> (Int, Int)
evalStep prog pc acc = case prog V.! pc of
                         Instruction NOOP _ -> (pc + 1, acc)
                         Instruction ACC x  -> (pc + 1, acc + x)
                         Instruction JMP x  -> (pc + x, acc)

run :: Program -> [(Int, Int)]
run prog = iterate (uncurry (evalStep prog)) (0, 0)

part1 :: Program -> Int
part1 = fromLeft 0 . loopOrTerminate

loopOrTerminate :: Program -> Either Int Int
loopOrTerminate = f mempty . run
  where
    f _ ((601, x):_) = Right x
    f s ((pc, x):xs)
      | pc `Set.member` s = Left x
      | otherwise = f (Set.insert pc s) xs
    f _ _ = error "impossible"

bruteforce :: Program -> [Program]
bruteforce = fmap V.fromList . go . V.toList
  where
    go :: [Instruction] -> [[Instruction]]
    go []                          = []
    go (i@(Instruction NOOP x):xs) = [Instruction JMP x:xs] <> fmap (i:) (go xs)
    go (i@(Instruction JMP x):xs)  = [Instruction NOOP x:xs] <> fmap (i:) (go xs)
    go (x:xs)                      = fmap (x:) (go xs)

part2 :: Program -> Int
part2 = head . rights . fmap loopOrTerminate . bruteforce
