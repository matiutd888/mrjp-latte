module Compile where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Grammar.AbsLatte as A
import Utils
import Utils64 (X64Code (codeLines))

type Loc = Integer

type StmtTEval a = StateT Env (ExceptT String IO) a

data LabelWriter = LWriter
  { lFunName :: String,
    lCounter :: Int
  }

data Env = Env
  { eVarLocs :: M.Map A.UIdent Loc, -- Location relative to rbp
    eVarTypes :: M.Map A.UIdent A.Type,
    eCurrClass :: Maybe A.UIdent,
    eClasses :: M.Map A.UIdent ClassType,
    eFunctions :: M.Map A.UIdent A.Type,
    eWriter :: LabelWriter
  }

initEnv :: M.Map A.UIdent A.Type -> M.Map A.UIdent ClassType -> Env
initEnv functions classes =
  Env
    { eVarLocs = M.empty,
      eVarTypes = M.empty,
      eCurrClass = Nothing,
      eClasses = classes,
      eFunctions = functions,
      eWriter = LWriter "" 0
    }

compileProgram :: A.Program -> StmtTEval String
compileProgram p = do
  code <- compileProgramHelp p
  return $ reverse $ T.unpack $ toLazyText $ codeLines code

compileProgramHelp :: A.Program -> StmtTEval X64Code
compileProgramHelp (A.ProgramT _ topdefs) = undefined

runStmtTEval :: Env -> StmtTEval a -> IO (Either String (a, Env))
runStmtTEval env e = runExceptT (runStateT e env)

runCompileProgram :: M.Map A.UIdent A.Type -> M.Map A.UIdent ClassType -> A.Program -> IO (Either String (String, Env))
runCompileProgram functions classes p = runStmtTEval (initEnv functions classes) (compileProgram p)
