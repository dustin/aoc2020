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

-- | readProgram parses a file of instructions into a Program.
readProgram :: FilePath -> IO Program
readProgram = fmap V.fromList . parseFile (some parseInstr)

-- | Program state is the PC and Accumulator value
type ProgramState = (Int, Int)

-- | evalStep executes a single instruction from a given state and returns the new state.
evalStep :: Program -> ProgramState -> ProgramState
evalStep prog (pc, acc) = case prog V.! pc of
                            Instruction NOOP _ -> (pc + 1, acc)
                            Instruction ACC x  -> (pc + 1, acc + x)
                            Instruction JMP x  -> (pc + x, acc)

-- | Run returns a continuous stream of program states from an initial state.
run :: Program -> [ProgramState]
run prog = iterate (evalStep prog) (0, 0)

-- | loopOrTerminate returns either a Left program state at a the
-- point *before* a loop occurs, or the Right final accumulator state.
loopOrTerminate :: Program -> Either ProgramState Int
loopOrTerminate program = f mempty . run $ program
  where
    end = length program
    f _ ((n, x):_)
      | n == end = Right x
    f s ((opc,_):n@(npc, x):xs)
      | npc `Set.member` s = Left (opc, x)
      | otherwise = f (Set.insert npc s) (n:xs)
    f _ _ = error "impossible"
