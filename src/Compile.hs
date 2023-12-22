{-# OPTIONS_GHC -Wno-type-defaults #-}

module Compile where

import qualified Control.Applicative as DM
import Control.Arrow (Arrow (second))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Bool as A
import qualified Data.DList as DList
import qualified Data.DList as DList.DList
import qualified Data.DList as U
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Maybe as DM
import Data.Text.Internal.Fusion (Step (Done))
import Data.Text.Internal.Fusion.Size (Size)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified GHC.Generics as U
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte (printTree)
import System.Posix.Internals (puts)
import Text.Read (Lexeme (String))
import Utils
import UtilsX86 (codeLines)
import qualified UtilsX86 as U

type Loc = Int

type StmtTEval a = StateT Env (ExceptT String IO) a

type ExprTEval a = StateT Env (ExceptT String IO) a

data LabelWriter = LWriter
  { lFunName :: String,
    lClassName :: String,
    lCounter :: Int
  }

labelReturn :: LabelWriter -> String
labelReturn (LWriter f c _) = c ++ "$" ++ f ++ "return"

getNewLabel :: String -> StmtTEval String
getNewLabel hint = do
  env <- get
  let lw = eWriter env
  let ret = getLabel lw hint
  put $
    env
      { eWriter = advanceLabel lw
      }
  return ret
  where
    advanceLabel :: LabelWriter -> LabelWriter
    advanceLabel l =
      l
        { lCounter = lCounter l + 1
        }

    getLabel :: LabelWriter -> String -> String
    getLabel l customHint = lClassName l ++ "$" ++ lFunName l ++ "$" ++ show (lCounter l) ++ "$" ++ customHint

data Env = Env
  { eCurrClass :: Maybe A.UIdent,
    eClasses :: M.Map A.UIdent ClassType,
    eFunctions :: M.Map A.UIdent A.Type,
    eVarLocs :: M.Map A.UIdent Loc, -- Location relative to ebp
    eVarTypes :: M.Map A.UIdent A.Type,
    eWriter :: LabelWriter,
    eLocalVarsBytesCounter :: Int,
    eStringConstants :: M.Map String String
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
      eLocalVarsBytesCounter = 0,
      eStringConstants = M.empty
    }

getTmpRegister :: StmtTEval U.Register
getTmpRegister = return "ecx"

getTwoTmpRegisters :: StmtTEval (U.Register, U.Register)
getTwoTmpRegisters = return ("ecx", "edi")

moveLocalVariableAddressToRegister :: Int -> U.Register -> StmtTEval U.X86Code
moveLocalVariableAddressToRegister offsetRelativeToFR reg = do
  return $ U.instrsToCode [U.Mov (U.Reg reg) (U.Reg U.frameRegister), U.Add (U.Reg reg) (U.Constant offsetRelativeToFR)]

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
      U.Sub (U.Reg U.stackRegister) $ U.Constant bytesForLocals
    ]

epilogue :: U.X86Code
epilogue =
  U.instrsToCode
    [ U.Mov (U.Reg U.stackRegister) (U.Reg U.frameRegister),
      U.Pop (U.Reg U.frameRegister),
      U.Return
    ]

runExprTEval :: Env -> ExprTEval a -> IO (Either String (a, Env))
runExprTEval env e = runExceptT (runStateT e env)

liftExprTEval :: ExprTEval a -> StmtTEval a
liftExprTEval e = do
  env <- get
  (a, newEnv) <- liftIO (runExprTEval env e) >>= liftEither
  put newEnv
  return a

evalExpr :: A.Expr -> ExprTEval (U.X86Code, A.Type)
evalExpr (A.EOr _ e1 e2) = do
  (e1Code, _) <- evalExpr e1
  (e2Code, _) <- evalExpr e2
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popValuesToTmpRegisters = U.instrsToCode [U.Pop (U.Reg tmp2), U.Pop (U.Reg tmp1)]
  let orRegisters = U.instrToCode $ U.Or (U.Reg tmp1) (U.Reg tmp2)
  let pushResult = U.instrToCode $ U.Push $ U.Reg tmp1
  return (pushResult <> orRegisters <> popValuesToTmpRegisters <> e2Code <> e1Code, A.TBool noPos)
evalExpr (A.EAnd _ e1 e2) = do
  (e1Code, _) <- evalExpr e1
  (e2Code, _) <- evalExpr e2
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popValuesToTmpRegisters = U.instrsToCode [U.Pop (U.Reg tmp2), U.Pop (U.Reg tmp1)]
  let andRegisters = U.instrToCode $ U.And (U.Reg tmp1) (U.Reg tmp2)
  let pushResult = U.instrToCode $ U.Push $ U.Reg tmp1
  return (pushResult <> andRegisters <> popValuesToTmpRegisters <> e2Code <> e1Code, A.TBool noPos)
evalExpr (A.Not _ e) = do
  (exprCode, t) <- evalExpr e
  let xorTopOfTheStack = U.instrToCode $ U.Xor (U.SimpleMem U.stackRegister 0) (U.Constant 1)
  return (xorTopOfTheStack <> exprCode, t)
evalExpr (A.Neg _ e) = do
  (exprCode, t) <- evalExpr e
  let negateValue = U.instrToCode $ U.Neg $ U.SimpleMem U.stackRegister 0
  return (negateValue <> exprCode, t)
evalExpr (A.EApp _ f exprs) = do
  retType <- gets ((fromJust . M.lookup f) . eFunctions)
  let reverseExprs = reverse exprs
  result <- mapM evalExpr reverseExprs
  let pushAllArgumentsToTheStack = mconcat $ map fst result
  let callF = U.instrToCode $ U.Call $ printTree f
  let popArgumentsFromStack = mconcat $ replicate (length exprs) U.popToNothing
  let pushReturnValue = U.instrsToCode $ [U.Push $ U.Reg U.resultRegister]
  return (pushReturnValue <> popArgumentsFromStack <> callF <> pushAllArgumentsToTheStack, retType)
evalExpr (A.EString _ s) = do
  newStringLabel <- getNewLabel "stringLit"
  env <- get
  put $ env {eStringConstants = M.insert newStringLabel s (eStringConstants env)}
  -- TODO maybe put the constant on heap?
  return (U.instrsToCode [U.Push $ U.StringConstant newStringLabel], A.TStr noPos)
evalExpr (A.ELitInt _ i) = return (U.instrsToCode [U.Push $ U.Constant $ fromIntegral i], A.TInt noPos)
evalExpr (A.ELitFalse _) = return (U.instrsToCode [U.Push $ U.Constant 0], A.TBool noPos)
evalExpr (A.ELitTrue _) = return (U.instrsToCode [U.Push $ U.Constant 1], A.TBool noPos)
evalExpr A.ECastNull {} = undefined
evalExpr A.EMemberCall {} = undefined
evalExpr A.EMember {} = undefined
evalExpr (A.ESelf _) = undefined
evalExpr (A.ENewObject _ _) = undefined
evalExpr _ = undefined

getNumberOfBytesForLocals :: A.Stmt -> Int
getNumberOfBytesForLocals = fst . getNumberOfBytesForLocalsHelper 0 0
  where
    getNumberOfBytesForLocalsHelper :: Int -> Int -> A.Stmt -> (Int, Int)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SBStmt _ (A.SBlock _ stmts)) =
      let (resMax, _resDeclCtr) = foldl (uncurry getNumberOfBytesForLocalsHelper) (currMax, currDecl) stmts
       in (resMax, currDecl)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SDecl _ t items) =
      let bytes = U.sizeOfTypeBytes t
       in let newCurr = currDecl + bytes * length items
           in (max currMax newCurr, newCurr)
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SCond _ _ s) = let (newMax, _newDecl) = getNumberOfBytesForLocalsHelper currMax currDecl s in (newMax, currDecl)
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
        eVarTypes = eVarTypes env,
        eLocalVarsBytesCounter = eLocalVarsBytesCounter env
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

    -- TODO init variables here.
    handleItem :: A.Item -> StmtTEval U.X86Code
    handleItem (A.SNoInit _ ident) = handleDecl ident >> return mempty
    handleItem (A.SInit _ ident expr) = do
      handleDecl ident
      (exprCode, _) <- liftExprTEval (evalExpr expr)
      env <- get
      let moveValueToLocationCode = U.instrToCode $ U.Pop $ U.SimpleMem U.frameRegister (eVarLocs env M.! ident)
      return $ moveValueToLocationCode <> exprCode
generateCode (A.SRet _ e) = do
  (exprCode, _) <- liftExprTEval (evalExpr e)
  let popResult = U.instrToCode $ U.Pop $ U.Reg U.resultRegister
  lWriter <- gets eWriter
  let lReturn = labelReturn lWriter
  let jumpToEpilogue = U.instrToCode $ U.Jmp lReturn
  return $ jumpToEpilogue <> popResult <> exprCode
generateCode (A.SVRet _) = do
  lWriter <- gets eWriter
  let lReturn = labelReturn lWriter
  let jmpToEpilogue = U.instrToCode $ U.Jmp lReturn
  return jmpToEpilogue
generateCode (A.SAss _ e1 e2) = do
  putOnStack <- getLValueAddressOnStack e1
  (exprCode, _) <- liftExprTEval (evalExpr e2)
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popValuesToTmpRegisters = U.instrsToCode [U.Pop (U.Reg tmp2), U.Pop (U.Reg tmp1)]
  let movValueToLocation = U.instrsToCode [U.Mov (U.SimpleMem tmp1 0) (U.Reg tmp2)]
  return $ movValueToLocation <> popValuesToTmpRegisters <> exprCode <> putOnStack
generateCode (A.SIncr _ e) = do
  putOnStack <- getLValueAddressOnStack e
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popAddressToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let movValueToRegister = U.instrsToCode [U.Mov (U.Reg tmp2) (U.SimpleMem tmp1 0)]
  let incrementValue = U.instrsToCode [U.Add (U.Reg tmp2) (U.Constant 1)]
  let movValueToLocation = U.instrsToCode [U.Mov (U.SimpleMem tmp1 0) (U.Reg tmp2)]
  return $ movValueToLocation <> incrementValue <> movValueToRegister <> popAddressToRegister <> putOnStack
generateCode (A.SDecr _ e) = do
  putOnStack <- getLValueAddressOnStack e
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popAddressToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let movValueToRegister = U.instrsToCode [U.Mov (U.Reg tmp2) (U.SimpleMem tmp1 0)]
  let decrementValue = U.instrsToCode [U.Add (U.Reg tmp2) (U.Constant (-1))]
  let movValueToLocation = U.instrsToCode [U.Mov (U.SimpleMem tmp1 0) (U.Reg tmp2)]
  return $ movValueToLocation <> decrementValue <> movValueToRegister <> popAddressToRegister <> putOnStack
generateCode (A.SCond _ e s) = do
  skipStatementLabel <- getNewLabel "skipStatement"
  (exprCode, _) <- liftExprTEval (evalExpr e)
  tmp1 <- getTmpRegister
  let popResultToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let compareWithZero = U.instrsToCode [U.Cmp (U.Reg tmp1) (U.Constant 0)]
  let jumpIfEqual = U.instrsToCode [U.Je skipStatementLabel]
  stmtCode <- generateCode s
  let labelInCode = U.instrsToCode [U.Label skipStatementLabel]
  return $ labelInCode <> stmtCode <> jumpIfEqual <> compareWithZero <> popResultToRegister <> exprCode
generateCode (A.SCondElse _ e s1 s2) = do
  labelFalse <- getNewLabel "lFalse"
  labelEnd <- getNewLabel "lEnd"
  (exprCode, _) <- liftExprTEval (evalExpr e)
  tmp1 <- getTmpRegister
  let popResultToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let compareWithZero = U.instrsToCode [U.Cmp (U.Reg tmp1) (U.Constant 0)]
  let jumpToFalseIfEqual = U.instrsToCode [U.Je labelFalse]
  s1Code <- generateCode s1
  let jumpToEnd = U.instrsToCode [U.Jmp labelEnd]
  let labelFalseCode = U.instrsToCode [U.Label labelFalse]
  s2Code <- generateCode s2
  let labelEndCode = U.instrsToCode [U.Label labelEnd]
  return $ labelEndCode <> s2Code <> labelFalseCode <> jumpToEnd <> s1Code <> jumpToFalseIfEqual <> compareWithZero <> popResultToRegister <> exprCode
generateCode (A.SWhile _ e s) = do
  l1 <- getNewLabel "L1"
  l2 <- getNewLabel "L2"
  let goToL2Code = U.instrsToCode [U.Jmp l2]
  let l1Code = U.instrsToCode [U.Label l1]
  bodyCode <- generateCode s
  let l2Code = U.instrsToCode [U.Label l2]
  (exprCode, _) <- liftExprTEval (evalExpr e)
  tmp1 <- getTmpRegister
  let popResultToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let compareWithOne = U.instrsToCode [U.Cmp (U.Reg tmp1) (U.Constant 1)]
  let jumpToL1IfEqual = U.instrsToCode [U.Je l1]
  return $ jumpToL1IfEqual <> compareWithOne <> popResultToRegister <> exprCode <> l2Code <> bodyCode <> l1Code <> goToL2Code
generateCode (A.SExp _ e) = do
  (exprCode, _) <- liftExprTEval (evalExpr e)
  let popToNothing = U.popToNothing
  return $ popToNothing <> exprCode

getLValueAddressOnStack :: A.Expr -> StmtTEval U.X86Code
getLValueAddressOnStack (A.EVar _ ident) = do
  m <- gets eVarLocs
  let loc = m M.! ident
  tmp <- getTmpRegister
  moveToRegister <- moveLocalVariableAddressToRegister loc tmp
  let pushAddress = U.instrsToCode [U.Push $ U.Reg tmp]
  return $ pushAddress <> moveToRegister
getLValueAddressOnStack _ = undefined

compileFunction :: A.UIdent -> [A.UIdent] -> A.Block -> StmtTEval U.X86Code
compileFunction name idents body = do
  env <- get
  let (A.TFun _ _retType argTypes) = eFunctions env M.! name
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
  put newEnv
  code <- generateCode (A.SBStmt noPos body)
  put env
  return $ funEpilogue <> U.instrToCode (U.Label (labelReturn funLabelWriter)) <> code <> funPrologue

compileClass :: A.UIdent -> StmtTEval String
compileClass = undefined

compileTopDef :: A.TopDef -> StmtTEval String
compileTopDef (A.TopFuncDef _ (A.FunDefT _ _retType _uident _args _block)) = undefined
compileTopDef _ = undefined

compileProgram :: A.Program -> StmtTEval String
compileProgram _ = undefined

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
