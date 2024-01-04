{-# OPTIONS_GHC -Wno-type-defaults #-}

module Compile where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.DList as DList
import Data.List (intercalate, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Maybe as DM
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte (printTree)
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
  deriving (Show)

labelReturn :: LabelWriter -> String
labelReturn (LWriter f c _) = "label" ++ c ++ "$" ++ f ++ "return"

labelVTable :: String -> String
labelVTable className = "label" ++ className ++ "$" ++ "vTable"

-- TODO dont let user overshadow my helper function, think about how to do it!
labelFunction :: Maybe String -> String -> String
labelFunction Nothing "main" = "main"
labelFunction Nothing x = if x `elem` U.helpers then x else _labelFunctionHelper Nothing x
labelFunction c x = _labelFunctionHelper c x

_labelFunctionHelper :: Maybe String -> String -> String
_labelFunctionHelper c f = "f" ++ DM.fromMaybe "" c ++ "$" ++ f ++ "$" ++ "function"

labelEmptyStr :: String
labelEmptyStr = "label$$$EmptystringLit"

defaultWriter :: LabelWriter
defaultWriter = LWriter "" "" 0

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
    getLabel l customHint = "label" ++ lClassName l ++ "$" ++ lFunName l ++ "$" ++ show (lCounter l) ++ "$" ++ customHint

data ClassInMemory = CMem
  { cAttrOffsets :: M.Map A.UIdent Loc,
    cfunIndexInVTable :: M.Map A.UIdent (Int, String),
    cVTableLocationOffset :: Loc,
    cVTableLabel :: String,
    cNumberOfVTableEntries :: Int,
    cOffsetSize :: Int
  }
  deriving (Show)

data Env = Env
  { eCurrClass :: Maybe (A.UIdent, Loc),
    eClassesLayout :: M.Map A.UIdent ClassInMemory,
    eClasses :: M.Map A.UIdent ClassType,
    eFunctions :: M.Map A.UIdent A.Type,
    eVarLocs :: M.Map A.UIdent Loc, -- Location relative to ebp
    eVarTypes :: M.Map A.UIdent A.Type,
    eWriter :: LabelWriter,
    eLocalVarsBytesCounter :: Int,
    eStringConstants :: M.Map String String
  }
  deriving (Show)

initEnv :: M.Map A.UIdent A.Type -> M.Map A.UIdent ClassType -> Env
initEnv functions classes =
  Env
    { eCurrClass = Nothing,
      eClasses = classes,
      eFunctions = functions,
      eClassesLayout = M.empty,
      eVarLocs = M.empty,
      eVarTypes = M.empty,
      eWriter = defaultWriter,
      eLocalVarsBytesCounter = 0,
      eStringConstants = M.fromList [("", labelEmptyStr)]
    }

getTmpRegister :: StmtTEval U.Register
getTmpRegister = return "ecx"

getTwoTmpRegisters :: StmtTEval (U.Register, U.Register)
getTwoTmpRegisters = return ("ecx", "edi")

moveLocalVariableAddressToRegister :: Int -> U.Register -> StmtTEval U.X86Code
moveLocalVariableAddressToRegister offsetRelativeToFR reg =
  return $ U.instrsToCode [U.Lea (U.Reg reg) (U.SimpleMem U.frameRegister offsetRelativeToFR)]

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

getExpressionsValuesInRegisters :: A.Expr -> A.Expr -> ExprTEval (U.X86Code, U.Register, U.Register)
getExpressionsValuesInRegisters e1 e2 = do
  (e1Code, _) <- evalExpr e1
  (e2Code, _) <- evalExpr e2
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popValuesToTmpRegisters = U.instrsToCode [U.Pop (U.Reg tmp2), U.Pop (U.Reg tmp1)]
  return (popValuesToTmpRegisters <> e2Code <> e1Code, tmp1, tmp2)

getExpressionsValuesInRegistersWithType :: A.Expr -> A.Expr -> ExprTEval (A.Type, A.Type, U.X86Code, U.Register, U.Register)
getExpressionsValuesInRegistersWithType e1 e2 = do
  (e1Code, t1) <- evalExpr e1
  (e2Code, t2) <- evalExpr e2
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popValuesToTmpRegisters = U.instrsToCode [U.Pop (U.Reg tmp2), U.Pop (U.Reg tmp1)]
  return (t1, t2, popValuesToTmpRegisters <> e2Code <> e1Code, tmp1, tmp2)

addStrings :: U.Register -> U.Register -> ExprTEval U.X86Code
addStrings tmp1 tmp2 = do
  let pushSecondArgument = U.instrToCode $ U.Push $ U.Reg tmp2
  let pushFirstArgument = U.instrToCode $ U.Push $ U.Reg tmp1
  let callUtilsMethod = U.instrToCode $ U.Call U.helperConcatStrings
  let popArguments = U.popToNothing <> U.popToNothing
  let pushReturnValue = U.instrsToCode [U.Push $ U.Reg U.resultRegister]
  return $ pushReturnValue <> popArguments <> callUtilsMethod <> pushFirstArgument <> pushSecondArgument

compareStrings :: A.RelOp -> U.Register -> U.Register -> ExprTEval U.X86Code
compareStrings (A.EQU _) tmp1 tmp2 = do
  let pushSecondArgument = U.instrToCode $ U.Push $ U.Reg tmp2
  let pushFirstArgument = U.instrToCode $ U.Push $ U.Reg tmp1
  let callUtilsMethod = U.instrToCode $ U.Call U.helperStringsEqual
  let popArguments = U.popToNothing <> U.popToNothing
  let pushReturnValue = U.instrsToCode [U.Push $ U.Reg U.resultRegister]
  return $ pushReturnValue <> popArguments <> callUtilsMethod <> pushFirstArgument <> pushSecondArgument
compareStrings (A.NE _) tmp1 tmp2 = do
  let pushSecondArgument = U.instrToCode $ U.Push $ U.Reg tmp2
  let pushFirstArgument = U.instrToCode $ U.Push $ U.Reg tmp1
  let callUtilsMethod = U.instrToCode $ U.Call U.helperStringsEqual
  let popArguments = U.popToNothing <> U.popToNothing
  let xorResult = U.instrToCode $ U.Xor (U.Reg U.resultRegister) (U.Constant 1)
  let pushReturnValue = U.instrsToCode [U.Push $ U.Reg U.resultRegister]
  return $ pushReturnValue <> xorResult <> popArguments <> callUtilsMethod <> pushFirstArgument <> pushSecondArgument
compareStrings _ _ _ = undefined

-- Cmp x1 x2
-- j cnmpFalse
-- code if comparison is true
-- cmpFalse:
-- skipFalse

compareValuesHelper :: String -> (String -> U.Asm) -> ExprTEval U.X86Code
compareValuesHelper hint jumpIfComparisonIsFalseCreator = do
  labelFalse <- getNewLabel $ hint ++ "false"
  skipFalse <- getNewLabel $ hint ++ "skip"
  let jumpIfComparisonIsFalse = U.instrsToCode [jumpIfComparisonIsFalseCreator labelFalse]
  let codeIfComparisonWasTrue = U.instrsToCode [U.Push (U.Constant 1), U.Jmp skipFalse]
  let labelFalseCode = U.instrToCode $ U.Label labelFalse
  let codeIfComparisonWasFalse = U.instrToCode $ U.Push $ U.Constant 0
  let labelSkipFalseCode = U.instrToCode $ U.Label skipFalse
  return $ labelSkipFalseCode <> codeIfComparisonWasFalse <> labelFalseCode <> codeIfComparisonWasTrue <> jumpIfComparisonIsFalse

compareValues :: A.RelOp -> ExprTEval U.X86Code
compareValues (A.EQU _) = compareValuesHelper "equ" U.Jne
compareValues (A.NE _) = compareValuesHelper "ne" U.Je
compareValues (A.LE _) = compareValuesHelper "le" U.Jg
compareValues (A.GE _) = compareValuesHelper "ge" U.Jl
compareValues (A.LTH _) = compareValuesHelper "lth" U.Jge
compareValues (A.GTH _) = compareValuesHelper "gth" U.Jle

evalExpr :: A.Expr -> ExprTEval (U.X86Code, A.Type)
evalExpr (A.EVar _ x) = do
  locationM <- gets (M.lookup x . eVarLocs)
  case locationM of
    Just l -> getLocalVar l
    Nothing -> evalExpr (A.EMember noPos (A.ESelf noPos) x)
  where
    getLocalVar :: Loc -> ExprTEval (U.X86Code, A.Type)
    getLocalVar location = do
      t <- gets (fromJust . M.lookup x . eVarTypes)
      let pushValue = U.instrsToCode [U.Push $ U.SimpleMem U.frameRegister location]
      return (pushValue, t)
evalExpr (A.ERel _ e1 erel e2) = do
  (t1, _t2, valuesInRegistersCode, tmp1, tmp2) <- getExpressionsValuesInRegistersWithType e1 e2
  case t1 of
    A.TStr _ -> do
      compareStringsCode <- compareStrings erel tmp1 tmp2
      return (compareStringsCode <> valuesInRegistersCode, A.TBool noPos)
    _ -> do
      let cmpRegisters = U.instrsToCode [U.Cmp (U.Reg tmp1) (U.Reg tmp2)]
      compareValuesCode <- compareValues erel
      return (compareValuesCode <> cmpRegisters <> valuesInRegistersCode, A.TBool noPos)
evalExpr (A.EAdd _ e1 (A.Minus _) e2) = do
  (valuesInRegistersCode, tmp1, tmp2) <- getExpressionsValuesInRegisters e1 e2
  let subRegisters = U.instrsToCode [U.Sub (U.Reg tmp1) (U.Reg tmp2)]
  let pushResult = U.instrToCode $ U.Push $ U.Reg tmp1
  return (pushResult <> subRegisters <> valuesInRegistersCode, A.TInt noPos)
evalExpr (A.EAdd _ e1 (A.Plus _) e2) = do
  (t1, _t2, valuesInRegistersCode, tmp1, tmp2) <- getExpressionsValuesInRegistersWithType e1 e2
  case t1 of
    A.TStr _ -> do
      addStringsCode <- addStrings tmp1 tmp2
      return (addStringsCode <> valuesInRegistersCode, t1)
    A.TInt _ -> do
      addIntCode <- addInt tmp1 tmp2
      return (addIntCode <> valuesInRegistersCode, t1)
    _ -> undefined
  where
    addInt :: U.Register -> U.Register -> ExprTEval U.X86Code
    addInt tmp1 tmp2 = do
      let addRegisters = U.instrsToCode [U.Add (U.Reg tmp1) (U.Reg tmp2)]
      let pushResult = U.instrToCode $ U.Push $ U.Reg tmp1
      return $ pushResult <> addRegisters
evalExpr (A.EMul _ e1 (A.Mod _) e2) = do
  (e1Code, _) <- evalExpr e1
  (e2Code, _) <- evalExpr e2
  let popDivisorToEcx = U.instrsToCode [U.Pop $ U.Reg "ecx"]
  let popDividentToEax = U.instrsToCode [U.Pop $ U.Reg "eax"]
  let prepareEdxValue = U.instrsToCode [U.Mov (U.Reg "edx") (U.Reg "eax"), U.Sar (U.Reg "edx") $ U.Constant 31]
  let divRegisters = U.instrToCode $ U.Idiv (U.Reg "ecx")
  let pushResult = U.instrToCode $ U.Push $ U.Reg "edx"
  return (pushResult <> divRegisters <> prepareEdxValue <> popDividentToEax <> popDivisorToEcx <> e2Code <> e1Code, A.TInt noPos)
evalExpr (A.EMul _ e1 (A.Div _) e2) = do
  (e1Code, _) <- evalExpr e1
  (e2Code, _) <- evalExpr e2
  let popDivisorToEcx = U.instrsToCode [U.Pop $ U.Reg "ecx"]
  let popDividentToEax = U.instrsToCode [U.Pop $ U.Reg "eax"]
  let prepareEdxValue = U.instrsToCode [U.Mov (U.Reg "edx") (U.Reg "eax"), U.Sar (U.Reg "edx") (U.Constant 31)]
  let divRegisters = U.instrToCode $ U.Idiv (U.Reg "ecx")
  let pushResult = U.instrToCode $ U.Push $ U.Reg "eax"
  return (pushResult <> divRegisters <> prepareEdxValue <> popDividentToEax <> popDivisorToEcx <> e2Code <> e1Code, A.TInt noPos)
evalExpr (A.EMul _ e1 (A.Times _) e2) = do
  (valuesInRegistersCode, tmp1, tmp2) <- getExpressionsValuesInRegisters e1 e2
  let mulRegisters = U.instrToCode $ U.Imul (U.Reg tmp1) (U.Reg tmp2)
  let pushResult = U.instrToCode $ U.Push $ U.Reg tmp1
  return (pushResult <> mulRegisters <> valuesInRegistersCode, A.TInt noPos)
evalExpr (A.EOr _ e1 e2) = do
  lOrTrue <- getNewLabel "LOrTrue"
  lOrEnd <- getNewLabel "lOrEnd"
  c1 <- evalBooleanExprHelp lOrTrue 1 e1
  c2 <- evalBooleanExprHelp lOrTrue 1 e2
  let cFalse = U.instrsToCode [U.Push (U.Constant 0), U.Jmp lOrEnd]
  let labelTrue = U.instrToCode $ U.Label lOrTrue
  let cTrue = U.instrsToCode [U.Push $ U.Constant 1]
  let labelEnd = U.instrToCode $ U.Label lOrEnd
  return (labelEnd <> cTrue <> labelTrue <> cFalse <> c2 <> c1, A.TBool noPos)
evalExpr (A.EAnd _ e1 e2) = do
  lAndFalse <- getNewLabel "LAndFalse"
  lAndEnd <- getNewLabel "LAndEnd"
  c1 <- evalBooleanExprHelp lAndFalse 0 e1
  c2 <- evalBooleanExprHelp lAndFalse 0 e2
  let cTrue = U.instrsToCode [U.Push (U.Constant 1), U.Jmp lAndEnd]
  let labelFalse = U.instrToCode $ U.Label lAndFalse
  let cFalse = U.instrsToCode [U.Push $ U.Constant 0]
  let labelEnd = U.instrToCode $ U.Label lAndEnd
  return (labelEnd <> cFalse <> labelFalse <> cTrue <> c2 <> c1, A.TBool noPos)
evalExpr (A.Not _ e) = do
  (exprCode, t) <- evalExpr e
  let xorTopOfTheStack = U.instrToCode $ U.Xor (U.SimpleMem U.stackRegister 0) (U.Constant 1)
  return (xorTopOfTheStack <> exprCode, t)
evalExpr (A.Neg _ e) = do
  (exprCode, t) <- evalExpr e
  let negateValue = U.instrToCode $ U.Neg $ U.SimpleMem U.stackRegister 0
  return (negateValue <> exprCode, t)
evalExpr (A.EApp _ fName expressions) = do
  env <- get
  let currClassFunction = (M.lookup fName . cfunIndexInVTable <$> ((eClassesLayout env) M.!)) . fst <$> (eCurrClass env)
  case currClassFunction of
    DM.Just (DM.Just _) -> evalExpr (A.EMemberCall noPos (A.ESelf noPos) fName expressions)
    _ -> evalGlobalFunction fName expressions
  where
    evalGlobalFunction f exprs = do
      A.TFun _ retType _ <- gets ((fromJust . M.lookup f) . eFunctions)
      let reverseExprs = reverse exprs
      result <- mapM evalExpr reverseExprs
      let pushAllArgumentsToTheStack = mconcat . reverse $ map fst result
      let callF = U.instrToCode $ U.Call $ labelFunction Nothing $ printTree f
      let popArgumentsFromStack = mconcat $ replicate (length exprs) U.popToNothing
      let pushReturnValue = U.instrsToCode [U.Push $ U.Reg U.resultRegister]
      return (pushReturnValue <> popArgumentsFromStack <> callF <> pushAllArgumentsToTheStack, retType)
evalExpr (A.EString _ s) = do
  label <- addLabelIfNotExist s
  return (U.instrsToCode [U.Push $ U.StringConstant label], A.TStr noPos)
  where
    addLabelIfNotExist :: String -> StmtTEval String
    addLabelIfNotExist stringConstant = do
      stringConstants <- gets eStringConstants
      if M.member stringConstant stringConstants then return (DM.fromJust $ M.lookup stringConstant stringConstants) else addNewLabel stringConstant

    addNewLabel :: String -> StmtTEval String
    addNewLabel stringConstant = do
      newLabel <- getNewLabel "stringLit"
      stringConstants <- gets eStringConstants
      let newM = M.insert stringConstant newLabel stringConstants
      env <- get
      put $
        env
          { eStringConstants = newM
          }
      return newLabel
evalExpr (A.ELitInt _ i) = return (U.instrsToCode [U.Push $ U.Constant $ fromIntegral i], A.TInt noPos)
evalExpr (A.ELitFalse _) = return (U.instrsToCode [U.Push $ U.Constant 0], A.TBool noPos)
evalExpr (A.ELitTrue _) = return (U.instrsToCode [U.Push $ U.Constant 1], A.TBool noPos)
--   data ClassInMemory = CMem
-- { cAttrOffsets :: M.Map A.UIdent Loc,
--   cfunIndexInVTable :: M.Map A.UIdent Int,
--   cVTableLocationOffset :: Loc,
--   cVTableLabel :: String,
--   cNumberOfVTableEntries :: Int,
--   cOffsetSize :: Int
-- }
-- deriving (Show)

evalExpr (A.ECastNull _ c) = return (U.instrsToCode [U.Push $ U.Constant 0], A.TClass noPos c)
evalExpr (A.EMemberCall _ e f exprs) = do
  let reverseExprs = reverse exprs
  result <- mapM evalExpr reverseExprs
  let pushAllArgumentsToTheStack = mconcat . reverse $ map fst result

  (exprCode, A.TClass _ className) <- evalExpr e

  env <- get
  let cLayout = eClassesLayout env M.! className
  let cType = eClasses env M.! className
  let A.TFun _ retType _ = cFuncs cType M.! f
  (tmp1, tmp2) <- getTwoTmpRegisters
  let movAddressOfClassToRegister = U.instrToCode $ U.Mov (U.Reg tmp2) (U.SimpleMem U.stackRegister 0)
  let movAddressOfVTableToRegister = U.instrToCode $ U.Mov (U.Reg tmp1) (U.SimpleMem tmp2 $ cVTableLocationOffset cLayout)
  let (indexOfFunction, _) = cfunIndexInVTable cLayout M.! f
  debug $ "Function " ++ printTree f ++ " index in vtable of class " ++ show className ++ ": " ++ show indexOfFunction
  let callIndirect = U.instrsToCode $ [U.CallIndirect (U.SimpleMem tmp1 (indexOfFunction * sizeOfVTablePointer))]
  let popArgumentsFromStack = mconcat $ replicate (length exprs + 1) U.popToNothing
  let pushReturnValue = U.instrsToCode [U.Push $ U.Reg U.resultRegister]
  return (pushReturnValue <> popArgumentsFromStack <> callIndirect <> movAddressOfVTableToRegister <> movAddressOfClassToRegister <> exprCode <> pushAllArgumentsToTheStack, retType)
evalExpr (A.EMember _ e ident) = do
  (exprCode, A.TClass _ className) <- evalExpr e
  env <- get
  debug $ "EMember, succesfully compiled " ++ show exprCode
  let cLayout = eClassesLayout env M.! className
  let cType = eClasses env M.! className
  tmp1 <- getTmpRegister
  let popAddressToRegister = U.instrToCode $ U.Pop (U.Reg tmp1)
  let offset = cAttrOffsets cLayout M.! ident
  let pushMemberValue = U.instrToCode $ U.Push (U.SimpleMem tmp1 offset)
  return (pushMemberValue <> popAddressToRegister <> exprCode, (cAttrs cType) M.! ident)
evalExpr (A.ESelf _) = do
  debug "comiling ESelf"
  (currClassName, currClassLocation) <- gets (fromJust . eCurrClass)
  let pushValue = U.instrsToCode [U.Push $ U.SimpleMem U.frameRegister currClassLocation]
  return (pushValue, A.TClass noPos currClassName)
evalExpr (A.ENewObject _ (A.TClass _ c)) = do
  env <- get
  let cMemoryLayout = eClassesLayout env M.! c
  let cData = eClasses env M.! c
  (codeThatCallsMalloc, registerWithClassAddress) <- allocMemoryForClass (cOffsetSize cMemoryLayout)
  codeThatFillsVTablePointer <- fillVTablePointer (cVTableLabel cMemoryLayout) (cVTableLocationOffset cMemoryLayout) registerWithClassAddress
  let fillAttributesCode = mconcat $ map (handleOffset registerWithClassAddress cData) (M.toList (cAttrOffsets cMemoryLayout))
  let pushAddress = U.instrToCode $ U.Push (U.Reg registerWithClassAddress)
  return (pushAddress <> fillAttributesCode <> codeThatFillsVTablePointer <> codeThatCallsMalloc, A.TClass noPos c)
  where
    allocMemoryForClass :: Int -> StmtTEval (U.X86Code, U.Register)
    allocMemoryForClass sizeOfClass = do
      let pushSize = U.instrToCode $ U.Push (U.Constant sizeOfClass)
      let callUtilsMethod = U.instrToCode $ U.Call U.helperMalloc
      let popArgument = U.popToNothing
      return (popArgument <> callUtilsMethod <> pushSize, U.resultRegister)
    fillVTablePointer :: String -> Int -> U.Register -> StmtTEval U.X86Code
    fillVTablePointer vTableLabel offset classAddress = do
      tmp1 <- getTmpRegister
      let loadEffectiveAddressToRegister = U.instrToCode (U.Lea (U.Reg tmp1) (U.OpLabel vTableLabel))
      let moveAddressToMemory = U.instrsToCode [U.Mov (U.SimpleMem classAddress offset) (U.Reg tmp1)]
      return $ moveAddressToMemory <> loadEffectiveAddressToRegister
    fillValue :: Int -> A.Type -> U.Register -> U.X86Code
    fillValue offset t classAddress = U.instrsToCode [U.Mov (U.SimpleMem classAddress offset) $ getDefaultValueByType t]
      where
        getDefaultValueByType (A.TInt _) = U.Constant 0
        getDefaultValueByType (A.TStr _) = U.StringConstant labelEmptyStr
        getDefaultValueByType (A.TBool _) = U.Constant 0
        getDefaultValueByType (A.TClass _ _) = U.Constant 0
        getDefaultValueByType _ = undefined

    handleOffset :: U.Register -> ClassType -> (A.UIdent, Int) -> U.X86Code
    handleOffset classAddress cData (ident, offset) = let varType = cAttrs cData M.! ident in fillValue offset varType classAddress
evalExpr (A.ENewObject _ _) = undefined

evalBooleanExprHelp :: String -> Int -> A.Expr -> StmtTEval U.X86Code
evalBooleanExprHelp l value e = do
  (eCode, _) <- liftExprTEval $ evalExpr e
  tmp1 <- getTmpRegister
  let popResultToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let compareWithValue = U.instrsToCode [U.Cmp (U.Reg tmp1) (U.Constant value)]
  let jumpToLabelIfEqual = U.instrsToCode [U.Je l]
  return $ jumpToLabelIfEqual <> compareWithValue <> popResultToRegister <> eCode

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
    getNumberOfBytesForLocalsHelper currMax currDecl (A.SWhile _ _ s) =
      let (newMax, _newDecl) = getNumberOfBytesForLocalsHelper currMax currDecl s in (newMax, currDecl)
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
generateCode (A.SEmpty _) = return mempty
generateCode (A.SBStmt _ (A.SBlock _ stmts)) = do
  env <- get
  code <- mconcat . reverse <$> mapM generateCode stmts
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

    addDeclToEnv :: A.UIdent -> StmtTEval ()
    addDeclToEnv ident = do
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
    handleItem (A.SNoInit p ident) = let expr = getDefaultValueExpressionByType t in handleItem (A.SInit p ident expr)
      where
        getDefaultValueExpressionByType (A.TInt _) = A.ELitInt noPos 0
        getDefaultValueExpressionByType (A.TStr _) = A.EString noPos ""
        getDefaultValueExpressionByType (A.TBool _) = A.ELitFalse noPos
        getDefaultValueExpressionByType (A.TClass _ c) = A.ECastNull noPos c
        getDefaultValueExpressionByType _ = undefined
    handleItem (A.SInit _ ident expr) = do
      (exprCode, _) <- liftExprTEval (evalExpr expr)
      addDeclToEnv ident
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
  debug "compiling sincr"
  putOnStack <- getLValueAddressOnStack e
  debug $ "done putOnStack" ++ show putOnStack
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popAddressToRegister = U.instrsToCode [U.Pop (U.Reg tmp1)]
  let movValueToRegister = U.instrsToCode [U.Mov (U.Reg tmp2) (U.SimpleMem tmp1 0)]
  let incrementValue = U.instrsToCode [U.Add (U.Reg tmp2) (U.Constant 1)]
  let movValueToLocation = U.instrsToCode [U.Mov (U.SimpleMem tmp1 0) (U.Reg tmp2)]
  debug $ "compiling sincr end" ++ (show $ movValueToLocation <> incrementValue <> movValueToRegister <> popAddressToRegister <> putOnStack)
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
  locationM <- gets (M.lookup ident . eVarLocs)
  case locationM of
    Just l -> getLocalVarAddressOnStack l
    Nothing -> getLValueAddressOnStack (A.EMember noPos (A.ESelf noPos) ident)
  where
    getLocalVarAddressOnStack :: Loc -> StmtTEval U.X86Code
    getLocalVarAddressOnStack loc = do
      tmp <- getTmpRegister
      moveToRegister <- moveLocalVariableAddressToRegister loc tmp
      let pushAddress = U.instrsToCode [U.Push $ U.Reg tmp]
      return $ pushAddress <> moveToRegister
getLValueAddressOnStack (A.EMember _ expr ident) = do
  (exprCode, A.TClass _ className) <- evalExpr expr
  env <- get
  let cLayout = eClassesLayout env M.! className
  (tmp1, tmp2) <- getTwoTmpRegisters
  let popAddressToRegister = U.instrToCode $ U.Pop (U.Reg tmp1)
  let offset = cAttrOffsets cLayout M.! ident
  let saveMemberAddressInRegister = U.instrToCode $ U.Lea (U.Reg tmp2) (U.SimpleMem tmp1 offset)
  let pushLValueAddressOnStack = U.instrToCode $ U.Push $ U.Reg tmp2
  return (pushLValueAddressOnStack <> saveMemberAddressInRegister <> popAddressToRegister <> exprCode)
getLValueAddressOnStack _ = undefined

cleanFunctionSpecificEnvVariables :: Env -> Env
cleanFunctionSpecificEnvVariables env =
  env
    { eCurrClass = Nothing,
      eVarLocs = M.empty,
      eLocalVarsBytesCounter = 0,
      eVarTypes = M.empty,
      eWriter = defaultWriter
    }

main :: String
main = labelFunction Nothing "main"

compileFunctionHelper :: Maybe String -> A.UIdent -> [A.Type] -> [A.UIdent] -> A.Block -> StmtTEval U.X86Code
compileFunctionHelper maybeClassName fName argTypes idents body = do
  env <- get
  debug $ "compiling function " ++ show fName
  let codeFunName = labelFunction maybeClassName (printTree fName)
  let numberOfBytesForLocals = getNumberOfBytesForLocals (A.SBStmt noPos body)
  let globalHeader = if codeFunName == main then U.instrToCode U.GlobalHeader else U.instrsToCode []
  let funLabel = U.instrToCode $ U.Label codeFunName
  let funPrologue = prologue numberOfBytesForLocals
  let funEpilogue = epilogue
  let funLabelWriter =
        LWriter
          { lFunName = printTree fName,
            lClassName = DM.fromMaybe "" maybeClassName,
            lCounter = 1
          }
  let updatedIdents = maybe [] (const [self]) maybeClassName ++ idents
  let updatedArgTypes = maybe [] ((: []) . A.TClass noPos . A.UIdent) maybeClassName ++ argTypes
  let funLocations = writeArgumentsToLocationsMap updatedArgTypes updatedIdents M.empty
  let funLocalTypes = writeArgumentsToTypesMap updatedArgTypes updatedIdents M.empty
  let funLocalVarsBytesCounter = 0
  debug "creating evalBodyEnv"
  let evalBodyEnv =
        env
          { eCurrClass = (\className -> (A.UIdent className, funLocations M.! self)) <$> maybeClassName,
            eVarLocs = funLocations,
            eVarTypes = funLocalTypes,
            eWriter = funLabelWriter,
            eLocalVarsBytesCounter = funLocalVarsBytesCounter
          }
  put evalBodyEnv
  debug $ "created evalBodyEnv" ++ show evalBodyEnv
  code <- generateCode (A.SBStmt noPos body)
  modify cleanFunctionSpecificEnvVariables
  debug $ "compiled function " ++ show fName ++ "\n" ++ show code
  return $ U.instrsToCode [U.Newline] <> funEpilogue <> U.instrToCode (U.Label (labelReturn funLabelWriter)) <> code <> funPrologue <> funLabel <> globalHeader <> U.instrsToCode [U.Newline]

compileClassFunction :: A.UIdent -> A.UIdent -> [A.UIdent] -> A.Block -> StmtTEval U.X86Code
compileClassFunction className fName idents body = do
  env <- get
  let cType = eClasses env M.! className
  let (A.TFun _ _retType argTypes) = cFuncs cType M.! fName
  compileFunctionHelper (Just $ printTree className) fName argTypes idents body

compileFunction :: A.UIdent -> [A.UIdent] -> A.Block -> StmtTEval U.X86Code
compileFunction fName idents body = do
  env <- get
  let (A.TFun _ _retType argTypes) = eFunctions env M.! fName
  compileFunctionHelper Nothing fName argTypes idents body

sizeOfVTablePointer :: Int
sizeOfVTablePointer = 4

createAndPutInEnvClassMemoryLayout :: A.UIdent -> StmtTEval ()
createAndPutInEnvClassMemoryLayout c = do
  cType <- gets (\e -> eClasses e M.! c)
  classesLayout <- gets eClassesLayout
  let parentLayout = (classesLayout M.!) <$> baseClass cType
  cMem <- createAndPutInEnvClassMemoryLayoutHelper parentLayout c
  debug $ "class " ++ printTree c ++ " layout in memory: " ++ show cMem
  env <- get
  put $
    env
      { eClassesLayout = M.insert c cMem (eClassesLayout env)
      }
  return ()
  where
    createAndPutInEnvClassMemoryLayoutHelper :: Maybe ClassInMemory -> A.UIdent -> StmtTEval ClassInMemory
    createAndPutInEnvClassMemoryLayoutHelper Nothing x = do
      let vTableLabel = labelVTable (printTree x)
      let vTableLocationOffset = 0
      cType <- gets ((fromJust . M.lookup x) . eClasses)
      let (attrOffsets, offsetSize) = fillLocalVarsOffsets sizeOfVTablePointer (cAttrs cType)
      let (numberOfEntriesInVTable, vTableIndexes) = assignIndexesToClassMethods (printTree x) 0 M.empty (cFuncs cType)
      let ret =
            CMem
              { cAttrOffsets = attrOffsets,
                cNumberOfVTableEntries = numberOfEntriesInVTable,
                cVTableLabel = vTableLabel,
                cVTableLocationOffset = vTableLocationOffset,
                cfunIndexInVTable = vTableIndexes,
                cOffsetSize = offsetSize
              }
      return ret
    createAndPutInEnvClassMemoryLayoutHelper (Just parentLayout) x = do
      let vTableLabel = labelVTable (printTree x)
      let vTableLocationOffset = 0
      cType <- gets ((fromJust . M.lookup x) . eClasses)
      let (attrOffsets, offsetSize) = fillLocalVarsOffsets (cOffsetSize parentLayout) (cAttrs cType)
      let (numberOfEntriesInVTable, vTableIndexes) = assignIndexesToClassMethods (printTree x) (cNumberOfVTableEntries parentLayout) (cfunIndexInVTable parentLayout) (cFuncs cType)
      let ret =
            CMem
              { cAttrOffsets = attrOffsets,
                cNumberOfVTableEntries = numberOfEntriesInVTable,
                cVTableLabel = vTableLabel,
                cVTableLocationOffset = vTableLocationOffset,
                cfunIndexInVTable = vTableIndexes,
                cOffsetSize = offsetSize
              }
      return ret

    fillLocalVarsOffsets :: Loc -> M.Map A.UIdent A.Type -> (M.Map A.UIdent Loc, Loc)
    fillLocalVarsOffsets startingOffset localVars = M.foldrWithKey addVariable (M.empty, startingOffset) localVars
      where
        addVariable :: A.UIdent -> A.Type -> (M.Map A.UIdent Loc, Loc) -> (M.Map A.UIdent Loc, Loc)
        addVariable ident t (m, l) =
          let sizeOfVar = U.sizeOfTypeBytes t
           in (M.insert ident l m, l + sizeOfVar)
    assignIndexesToClassMethods :: String -> Int -> M.Map A.UIdent (Int, String) -> M.Map A.UIdent A.Type -> (Int, M.Map A.UIdent (Int, String))
    assignIndexesToClassMethods className initialIndex initialFunctionsIndexes functionsToAdd = foldl addFunction (initialIndex, initialFunctionsIndexes) (M.keys functionsToAdd)
      where
        addFunction :: (Int, M.Map A.UIdent (Int, String)) -> A.UIdent -> (Int, M.Map A.UIdent (Int, String))
        addFunction (currIndex, currMap) newIdent = (currIndex + 1, M.insert newIdent (currIndex, className) currMap)

compileClass :: A.UIdent -> [A.ClassMember] -> StmtTEval U.X86Code
compileClass c classMembers = do
  debug $ "compiling class " ++ printTree c
  funDefCodeList <- mapM compileClassMethod classMembers
  let funDefCode = mconcat funDefCodeList
  debug $ "done compiling class " ++ printTree c ++ "\n" ++ show funDefCode
  return funDefCode
  where
    compileClassMethod :: A.ClassMember -> StmtTEval U.X86Code
    compileClassMethod (A.ClassMethodT _ (A.FunDefT _ _ fname args block)) = let argNames = map (\(A.ArgT _ _ x) -> x) args in compileClassFunction c fname argNames block
    compileClassMethod _ = return mempty

codeToStr :: U.X86Code -> String
codeToStr code =
  let cLines = reverse $ DList.toList (codeLines code)
   in let postprocessed = postProcess cLines
       in let cLinesStr = map U.instrToString postprocessed
           in intercalate "\n" cLinesStr

postProcess :: [U.Asm] -> [U.Asm]
postProcess x =
  let postprocessed = postprecessingHelper x
   in if x == postprocessed then postprocessed else postProcess postprocessed

postprecessingHelper :: [U.Asm] -> [U.Asm]
postprecessingHelper = changePushPopToMov . filterOutRedundantInstructions

isRedundant :: U.Asm -> Bool
isRedundant (U.Add _ (U.Constant 0)) = True
isRedundant (U.Sub _ (U.Constant 0)) = True
isRedundant (U.Imul _ (U.Constant 1)) = True
isRedundant (U.Mov (U.Reg x) (U.Reg y)) = x == y
isRedundant _ = False

filterOutRedundantInstructions :: [U.Asm] -> [U.Asm]
filterOutRedundantInstructions = filter (not . isRedundant)

changePushPopToMov :: [U.Asm] -> [U.Asm]
changePushPopToMov = changePushPopToMovHelper []
  where
    changePushPopToMovHelper :: [U.Asm] -> [U.Asm] -> [U.Asm]
    changePushPopToMovHelper res [] = reverse res
    changePushPopToMovHelper res ((U.Push x) : (U.Pop (U.Reg y)) : t) = changePushPopToMovHelper (U.Mov (U.Reg y) x : res) t
    changePushPopToMovHelper res (h : t) = changePushPopToMovHelper (h : res) t

getClassesInTopologicalOrder :: M.Map A.UIdent ClassType -> [A.UIdent]
getClassesInTopologicalOrder classesMap = reverse $ getClassesInTopologicalOrderHelper S.empty [] classesMap
  where
    getClassesInTopologicalOrderHelper usedAlready ret m =
      if M.null m
        then ret
        else
          let (classesToAdd, remaining) = M.partition doesntExtendsOrExtendsSomethingIn m
           in let classesToAddSet = M.keysSet classesToAdd
               in getClassesInTopologicalOrderHelper (S.union classesToAddSet usedAlready) (S.toList classesToAddSet ++ ret) remaining
      where
        doesntExtendsOrExtendsSomethingIn :: ClassType -> Bool
        doesntExtendsOrExtendsSomethingIn ClassType {baseClass = Nothing} = True
        doesntExtendsOrExtendsSomethingIn ClassType {baseClass = Just x} = S.member x usedAlready

-- compileProgram x = return $ U.instrToCode $ U.Add (U.Constant 0) (U.Constant 1)

compileProgram :: A.Program -> StmtTEval U.X86Code
compileProgram (A.ProgramT _ topdefs) = do
  env <- get
  debug $ "initial Environment " ++ show env
  let text = U.instrToCode U.Text
  let dataLabel = U.instrToCode U.Data
  -- return $ U.instrToCode $ U.Add (U.Constant 0) (U.Constant 1)
  classesDefCode <- compileClasses $ DM.mapMaybe filterClasses topdefs
  debug $ "compiling functions..."
  funDefCodeList <- mapM compileFunctionTopDef topdefs
  debug $ "done compiling functions..." ++ show funDefCodeList
  let funDefCode = mconcat funDefCodeList
  stringConstants <- gets $ M.toList . eStringConstants
  let constantsCode = mconcat $ map createStringConstantInCode stringConstants

  debug $ "compiling classes memory..."

  classesMemory <- gets $ M.toList . eClassesLayout
  let vtables = mconcat $ reverse $ map (uncurry getVTable) classesMemory
  debug $ "done compiling classes memory..."
  return $ funDefCode <> classesDefCode <> text <> constantsCode <> vtables <> dataLabel
  where
    compileFunctionTopDef :: A.TopDef -> StmtTEval U.X86Code
    compileFunctionTopDef (A.TopFuncDef _ (A.FunDefT _ _ name args block)) = let argNames = map (\(A.ArgT _ _ x) -> x) args in compileFunction name argNames block
    compileFunctionTopDef _ = return mempty

    -- Function to sort a list of tuples by the second element
    sortBySecond :: Ord b => [(a, b)] -> [(a, b)]
    sortBySecond = sortBy (comparing snd)

    getVTable :: A.UIdent -> ClassInMemory -> U.X86Code
    getVTable _ c =
      let x = M.toList (cfunIndexInVTable c)
       in let sorted = map (\(funName, (_, funClassName)) -> labelFunction (Just funClassName) (printTree funName)) (sortBySecond x)
           in U.instrToCode $ U.VTable (cVTableLabel c) sorted

    compileClassDef :: A.ClassDef -> A.UIdent -> StmtTEval U.X86Code
    compileClassDef (A.ClassDefT _ _ classMembers) ident = compileClass ident classMembers
    compileClassDef (A.ClassExtDefT _ _ _ classMembers) ident = compileClass ident classMembers

    createStringConstantInCode :: (String, String) -> U.X86Code
    createStringConstantInCode (constant, label) = U.instrToCode $ U.StringConstantDeclaration label constant

    compileClasses :: [A.ClassDef] -> StmtTEval U.X86Code
    compileClasses classesDefs = do
      env <- get
      let classesInTopologicalOrder = getClassesInTopologicalOrder (eClasses env)
      debug $ "classes in topological order " ++ show classesInTopologicalOrder
      mapM_ createAndPutInEnvClassMemoryLayout classesInTopologicalOrder
      let classesDefsMap = createClassDefsMap classesDefs
      codeList <- mapM (\x -> compileClassDef (fromJust $ M.lookup x classesDefsMap) x) classesInTopologicalOrder
      let concatedCode = mconcat codeList
      return concatedCode

    createClassDefsMap :: [A.ClassDef] -> M.Map A.UIdent A.ClassDef
    createClassDefsMap = createClassDefsMapHelper M.empty
      where
        createClassDefsMapHelper res [] = res
        createClassDefsMapHelper acc (p@(A.ClassExtDefT _ ident _ _) : t) = createClassDefsMapHelper (M.insert ident p acc) t
        createClassDefsMapHelper acc (p@(A.ClassDefT _ ident _) : t) = createClassDefsMapHelper (M.insert ident p acc) t

    filterClasses :: A.TopDef -> Maybe A.ClassDef
    filterClasses (A.TopClassDef _ x) = Just x
    filterClasses _ = Nothing

runStmtTEval :: Env -> StmtTEval a -> IO (Either String (a, Env))
runStmtTEval env e = runExceptT (runStateT e env)

runCompileProgram :: M.Map A.UIdent A.Type -> M.Map A.UIdent ClassType -> A.Program -> IO (Either String (String, Env))
runCompileProgram functions classes p = runStmtTEval (initEnv functions classes) (codeToStr <$> compileProgram p)
