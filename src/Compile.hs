module Compile where

import qualified Control.Applicative as DM
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.DList as DList
import qualified Data.DList as DList.DList
import qualified Data.DList as U
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte (printTree)
import System.Posix.Internals (puts)
import System.Process (CreateProcess (env))
import Utils
import UtilsX86 (codeLines)
import qualified UtilsX86 as U

type Loc = Int

type StmtTEval a = StateT Env (ExceptT String IO) a

type ExprTEval a = ReaderT Env (ExceptT String IO) a

data LabelWriter = LWriter
  { lFunName :: String,
    lClassName :: String,
    lCounter :: Int
  }

labelReturn :: LabelWriter -> String
labelReturn (LWriter f c _) = c ++ "$" ++ f ++ "return"

data Env = Env
  { eCurrClass :: Maybe A.UIdent,
    eClasses :: M.Map A.UIdent ClassType,
    eFunctions :: M.Map A.UIdent A.Type,
    eVarLocs :: M.Map A.UIdent Loc, -- Location relative to ebp
    eVarTypes :: M.Map A.UIdent A.Type,
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

getTmpRegister :: StmtTEval U.Register
getTmpRegister = return "ecx"

getTwoTmpRegisters :: StmtTEval (U.Register, U.Register)
getTwoTmpRegisters = return ("ecx", "edi")

-- moveStackToRegister :: U.Register -> U.X86Code
-- moveStackToRegister reg =
--   U.instrsToCode
--     [ U.Mov (U.Reg reg) (U.SimpleMem U.stackRegister 0)
--     ]

-- moveStackToLocationRelativeToFR :: Int -> StmtTEval U.X86Code
-- moveStackToLocationRelativeToFR offsetRelativeToFR = do
--   tmpReg <- getTmpRegister
--   return $
--     U.instrsToCode $
--       [ U.Mov (U.Reg tmpReg) (U.SimpleMem U.stackRegister 0),
--         U.Mov (U.SimpleMem U.frameRegister offsetRelativeToFR) (U.Reg tmpReg)
--       ]

prologue :: Int -> U.X86Code
prologue bytesForLocals =
  U.instrsToCode
    [ U.Push $ U.Reg U.frameRegister,
      U.Mov (U.Reg U.frameRegister) (U.Reg U.stackRegister),
      U.Sub (U.Reg U.stackRegister) $ U.Constant (show bytesForLocals)
    ]

epilogue :: U.X86Code
epilogue =
  U.instrsToCode
    [ U.Mov (U.Reg U.stackRegister) (U.Reg U.frameRegister),
      U.Pop (U.Reg U.frameRegister),
      U.Return
    ]

runExprTEval :: Env -> ExprTEval a -> IO (Either String a)
runExprTEval env e = runExceptT (runReaderT e env)

liftExprTEval :: ExprTEval a -> StmtTEval a
liftExprTEval e = do
  env <- get
  liftIO (runExprTEval env e) >>= liftEither

evalExpr :: A.Expr -> ExprTEval U.X86Code
evalExpr = undefined

getNumberOfBytesForLocals :: A.Stmt -> Int
getNumberOfBytesForLocals = fst . getNumberOfBytesForLocalsHelper 0 0
  where
    getNumberOfBytesForLocalsHelper :: Int -> Int -> A.Stmt -> (Int, Int)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SBStmt _ (A.SBlock _ stmts)) =
      let (resMax, resDeclCtr) = foldl (uncurry getNumberOfBytesForLocalsHelper) (currMax, currDecl) stmts
       in (resMax, currDecl)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SDecl _ t items) =
      let bytes = U.sizeOfTypeBytes t
       in let newCurr = currDecl + bytes * length items
           in (max currMax newCurr, newCurr)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SCond _ _ s) = let (newMax, newDecl) = getNumberOfBytesForLocalsHelper currMax currDecl s in (newMax, currDecl)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SCondElse _ _ s1 s2) =
      let res = map (getNumberOfBytesForLocalsHelper currMax currDecl) [s1, s2]
       in (maximum (map fst res), currDecl)
    getNumberOfBytesForLocalsHelper x y _ = (x, y)

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

generateCode :: A.Stmt -> StmtTEval U.X86Code
generateCode (A.SEmpty _) = return $ mempty
generateCode (A.SBStmt _ (A.SBlock _ stmts)) = do
  env <- get
  code <- mconcat <$> mapM generateCode stmts
  newEnv <- get
  put $
    newEnv
      { eVarLocs = eVarLocs env,
        eVarTypes = eVarTypes env
      }
  return code
generateCode (A.SDecl _ t items) = foldM addDeclCode mempty items
  where
    addDeclCode :: U.X86Code -> A.Item -> StmtTEval U.X86Code
    addDeclCode currCode item = do
      itemCode <- handleItem item
      return $ itemCode <> currCode

    handleDecl :: A.UIdent -> StmtTEval ()
    handleDecl ident = do
      ctr <- gets eLocalVarsBytesCounter
      let variableOffset = ctr - U.sizeOfTypeBytes t
      env <- get
      put
        env
          { eLocalVarsBytesCounter = variableOffset,
            eVarLocs = M.insert ident variableOffset (eVarLocs env),
            eVarTypes = M.insert ident t (eVarTypes env)
          }
      return ()

    handleItem :: A.Item -> StmtTEval U.X86Code
    handleItem (A.SNoInit _ ident) = handleDecl ident >> return mempty
    handleItem (A.SInit _ ident expr) = do
      handleDecl ident
      exprCode <- liftExprTEval (evalExpr expr)
      env <- get
      let moveValueToLocationCode = U.instrToCode $ U.Pop $ U.SimpleMem U.frameRegister (eVarLocs env M.! ident)
      return $ moveValueToLocationCode <> exprCode
-- instrToCode (U.Mov [], ) $ <> code
generateCode (A.SRet _ e) = do
  exprCode <- liftExprTEval (evalExpr e)
  let popResult = U.instrToCode $ U.Pop $ U.Reg U.resultRegister
  lWriter <- gets eWriter

  let lReturn = labelReturn lWriter
  let jumpToEpilogue = U.instrToCode $ U.Label lReturn
  return $ jumpToEpilogue <> popResult <> exprCode
generateCode (A.SVRet _) = do
  lWriter <- gets eWriter
  let lReturn = labelReturn lWriter
  return $ U.instrToCode $ U.Label lReturn
generateCode (A.SAss _ e1 e2) = do
  getLValueAddressCode <- getLValueFromExpression e1
  exprCode <- liftExprTEval (evalExpr e2)
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popValuesToTmpRegisters = U.instrsToCode [U.Pop (U.Reg tmp2), U.Pop (U.Reg tmp1)]
  let movValueToLocation = U.instrsToCode [U.Mov (U.SimpleMem tmp1 0) (U.Reg tmp2)]
  return $ movValueToLocation <> popValuesToTmpRegisters <> exprCode <> getLValueAddressCode
  where
    getLValueFromExpression :: A.Expr -> StmtTEval U.X86Code
    getLValueFromExpression (A.EVar _ ident) = do
      m <- gets eVarLocs
      let loc = m M.! ident
      tmp1 <- getTmpRegister
      let retCode = U.instrsToCode [U.Mov (U.Reg tmp1) (U.Reg U.frameRegister), U.Add (U.Reg tmp1) (U.Constant $ show loc), U.Push $ U.Reg tmp1]
      return retCode
    getLValueFromExpression _ = undefined
generateCode _ = undefined

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

  let funLocations = writeArgumentsToLocationsMap argTypes idents M.empty
  let funLocalTypes = writeArgumentsToTypesMap argTypes idents M.empty
  let funLocalVarsBytesCounter = 0
  let newEnv =
        env
          { eVarLocs = funLocations,
            eWriter = funLabelWriter,
            eLocalVarsBytesCounter = funLocalVarsBytesCounter,
            eVarTypes = funLocalTypes
          }
  code <- generateCode (A.SBStmt noPos body)
  return $ funEpilogue <> U.instrToCode (U.Label (labelReturn funLabelWriter)) <> code <> funPrologue

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
