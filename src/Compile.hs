module Compile where

import qualified Control.Applicative as DM
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.DList as DList.DList
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte (printTree)
import Utils
import UtilsX86 (codeLines)
import qualified UtilsX86 as U

type Loc = Int

type StmtTEval a = StateT Env (ExceptT String IO) a

data LabelWriter = LWriter
  { lFunName :: String,
    lClassName :: String,
    lCounter :: Int
  }

data Env = Env
  { eVarLocs :: M.Map A.UIdent Loc, -- Location relative to rbp
    eVarTypes :: M.Map A.UIdent A.Type,
    eCurrClass :: Maybe A.UIdent,
    eClasses :: M.Map A.UIdent ClassType,
    eFunctions :: M.Map A.UIdent A.Type,
    eWriter :: LabelWriter,
    eLocalVarsBytesCounter :: Int
  }

initEnv :: M.Map A.UIdent A.Type -> M.Map A.UIdent ClassType -> Env
initEnv functions classes =
  Env
    { eCurrClass = Nothing,
      eClasses = classes,
      eFunctions = functions,
      eVarLocs = M.empty,
      eVarTypes = M.empty,
      eWriter = LWriter "" "" 0,
      eLocalVarsBytesCounter = 0
    }

prologue :: Int -> U.X86Code
prologue bytesForLocals =
  U.X86Code
    { U.codeLines =
        DList.DList.fromList $
          reverse
            [ U.Push U.frameRegister,
              U.Mov U.frameRegister U.stackRegister,
              U.Sub U.stackRegister $ show bytesForLocals
            ]
    }

epilogue :: U.X86Code
epilogue =
  U.X86Code
    { codeLines =
        DList.DList.fromList $
          reverse
            [ U.Mov U.stackRegister U.frameRegister,
              U.Pop U.frameRegister,
              U.Return
            ]
    }

getNumberOfBytesForLocals :: A.Stmt -> Int
getNumberOfBytesForLocals (A.SBStmt _ (A.SBlock _ stmts)) = sum $ map getNumberOfBytesForLocals stmts
getNumberOfBytesForLocals (A.SDecl _ t items) =
  let bytes = U.sizeOfTypeBytes t
   in bytes * length items
getNumberOfBytesForLocals (A.SCond _ _ s) = getNumberOfBytesForLocals s
getNumberOfBytesForLocals (A.SCondElse _ _ s1 s2) = maximum $ map getNumberOfBytesForLocals [s1, s2]
getNumberOfBytesForLocals _ = 0

writeArgumentsToLocationsMap :: [A.Type] -> [A.UIdent] -> M.Map A.UIdent Int -> M.Map A.UIdent Int
writeArgumentsToLocationsMap types idents currMap = fst $ foldl f (currMap, U.returnAddressOffset) $ zip types idents
  where
    f :: (M.Map A.UIdent Int, Int) -> (A.Type, A.UIdent) -> (M.Map A.UIdent Int, Int)
    f (m, counter) (t, ident) =
      let sizeOfType = U.sizeOfTypeBytes t
       in let argOffset = sizeOfType + counter
           in (M.insert ident argOffset m, argOffset)

writeArgumentsToTypesMap :: [A.Type] -> [A.UIdent] -> M.Map A.UIdent A.Type -> M.Map A.UIdent A.Type
writeArgumentsToTypesMap types idents currMap = foldl (\m (ident, t) -> M.insert ident t m) currMap $ zip idents types

compileFunction :: A.UIdent -> [A.UIdent] -> A.Block -> StmtTEval U.X86Code
compileFunction name idents body = do
  env <- get
  let (A.TFun _ retType argTypes) = eFunctions env M.! name
  let numberOfBytesForLocals = getNumberOfBytesForLocals (A.SBStmt noPos body)
  let funPrologue = prologue numberOfBytesForLocals
  let funEpilogue = epilogue
  let funLabelWriter =
        LWriter
          { lFunName = printTree name,
            lClassName = maybe "" printTree (eCurrClass env),
            lCounter = 1
          }

  let funLocations = writeArgumentsToLocationsMap argTypes idents DM.empty
  let funLocalTypes = writeArgumentsToTypesMap argTypes idents DM.empty
  let funLocalVarsBytesCounter = 0
  let newEnv =
        env
          { eVarLocs = funLocations,
            eWriter = funLabelWriter,
            eLocalVarsBytesCounter = funLocalVarsBytesCounter,
            eVarTypes = funLocalTypes
          }
  undefined

compileClass :: A.UIdent -> StmtTEval String
compileClass = undefined

compileTopDef :: A.TopDef -> StmtTEval String
compileTopDef (A.TopFuncDef _ (A.FunDefT _ retType uident args block)) = undefined
compileTopDef _ = undefined

compileProgram :: A.Program -> StmtTEval String
compileProgram p = undefined

-- where
--   compileAndAppendFunction :: String -> A.UIdent -> StmtTEval String
--   compileAndAppendFunction acc fun = do
--     compiledFunCode <- compileFunction fun
--     return $ compiledFunCode ++ acc

--   compileAndAppendClass :: String -> A.UIdent -> StmtTEval String
--   compileAndAppendClass acc fun = do
--     compiledClassCode <- compileClass fun
--     return $ compiledClassCode ++ acc

runStmtTEval :: Env -> StmtTEval a -> IO (Either String (a, Env))
runStmtTEval env e = runExceptT (runStateT e env)

runCompileProgram :: M.Map A.UIdent A.Type -> M.Map A.UIdent ClassType -> A.Program -> IO (Either String (String, Env))
runCompileProgram functions classes p = runStmtTEval (initEnv functions classes) (compileProgram p)
