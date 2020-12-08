module Computer (Operation(..), Instruction(..), Program, ProgramState,
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
-- A Left value indicates we've gone outside of program space (i.e., terminated?)
-- A Right value indicates we may continue.
evalStep :: Program -> Either ProgramState ProgramState -> Either ProgramState ProgramState
evalStep _ l@(Left _) = l
evalStep prog (Right st@(pc, acc)) = maybe (Left st) (Right . ex) (prog V.!? pc)
  where
    ex (Instruction NOOP _) = (pc+1, acc)
    ex (Instruction ACC x)  = (pc+1, acc+x)
    ex (Instruction JMP x)  = (pc+x, acc)

-- | Run returns a continuous stream of program states from an initial state.
run :: Program -> [Either ProgramState ProgramState]
run prog = iterate (evalStep prog) (Right (0, 0))

-- | loopOrTerminate returns either a Left program state at a the
-- point *before* a loop occurs, or the Right final accumulator state.
loopOrTerminate :: Program -> Either ProgramState Int
loopOrTerminate = fmap snd . f mempty . run
  where
    f _ (r@(Right _):(Left _):_) = r            -- normal termination
    f s (Right (opc,_):n@(Right (npc, x)):xs)
      | npc `Set.member` s = Left (opc, x)      -- looped
      | otherwise = f (Set.insert npc s) (n:xs)
    f _ _ = error "impossible"
