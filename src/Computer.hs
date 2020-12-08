module Computer (Operation(..), Instruction(..), Program,
                 readProgram, run, loopOrTerminate) where

import           Control.Applicative        ((<|>))
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

readProgram :: FilePath -> IO Program
readProgram = fmap V.fromList . parseFile (some parseInstr)

evalStep :: Program -> Int -> Int -> (Int, Int)
evalStep prog pc acc = case prog V.! pc of
                         Instruction NOOP _ -> (pc + 1, acc)
                         Instruction ACC x  -> (pc + 1, acc + x)
                         Instruction JMP x  -> (pc + x, acc)

run :: Program -> [(Int, Int)]
run prog = iterate (uncurry (evalStep prog)) (0, 0)

loopOrTerminate :: Program -> Either (Int,Int) Int
loopOrTerminate program = f mempty . run $ program
  where
    f _ ((n, x):_)
      | n == length program = Right x
    f s ((opc,_):n@(npc, x):xs)
      | npc `Set.member` s = Left (opc, x)
      | otherwise = f (Set.insert npc s) (n:xs)
    f _ _ = error "impossible"
