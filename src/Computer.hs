{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Computer (Operation(..), Program, ProgramState(..),
                 progPC, progAcc,
                 readProgram, run, loopOrTerminate) where

import           Control.Applicative        ((<|>))
import           Control.DeepSeq            (NFData (..))
import           Control.Lens
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Advent.AoC

data Operation = NOOP !Int
               | ACC !Int
               | JMP !Int
  deriving (Show, Generic)

instance NFData Operation

type Program = V.Vector Operation

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseInstr :: Parser Operation
parseInstr = NOOP <$> intop "nop"
         <|> ACC <$> intop "acc"
         <|> JMP <$> intop "jmp"
  where
    intop :: Parser a -> Parser Int
    intop n = lexeme n *> L.signed space L.decimal

-- | readProgram parses a file of instructions into a Program.
readProgram :: FilePath -> IO Program
readProgram = fmap V.fromList . parseFile (parseInstr `endBy` "\n")

data ProgramState = ProgramState {
  _progPC    :: Int
  , _progAcc :: Int
  } deriving (Show, Generic)

makeLenses ''ProgramState

emptyProgramState :: ProgramState
emptyProgramState = ProgramState 0 0

-- | evalStep executes a single instruction from a given state and returns the new state.
-- A Left value indicates we've gone outside of program space (i.e., terminated?)
-- A Right value indicates we may continue.
evalStep :: Program -> Either ProgramState ProgramState -> Either ProgramState ProgramState
evalStep _ l@(Left _) = l
evalStep prog (Right st@ProgramState{_progPC}) = maybe (Left st) (Right . ex) (prog V.!? _progPC)
  where
    ex (NOOP _) = st & progPC +~ 1
    ex (ACC x)  = st & progPC +~ 1 & progAcc +~ x
    ex (JMP x)  = st & progPC +~ x

-- | Run returns a continuous stream of program states from an initial state.
run :: Program -> [Either ProgramState ProgramState]
run prog = iterate (evalStep prog) (Right emptyProgramState)

-- | loopOrTerminate returns either a Left program state at a the
-- point *before* a loop occurs, or the Right final accumulator state.
loopOrTerminate :: Program -> Either ProgramState Int
loopOrTerminate = fmap _progAcc . f mempty . run
  where
    f _ (r@(Right _):(Left _):_) = r            -- normal termination
    f s (Right (ProgramState{_progPC=opc}):n@(Right (np@ProgramState{_progPC=npc}) ):xs)
      | npc `Set.member` s = Left (np{_progPC=opc})      -- looped
      | otherwise = f (Set.insert npc s) (n:xs)
    f _ _ = error "impossible"
