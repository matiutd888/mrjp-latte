{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalysis where

import Control.Exception (handle)
import Control.Monad (foldM, when)
import Control.Monad.Except
  ( ExceptT,
    Monad (return, (>>), (>>=)),
    MonadError (throwError),
    foldM,
    foldM_,
    liftEither,
    mapM_,
    runExceptT,
    unless,
    when,
  )
import Control.Monad.Identity
import Control.Monad.RWS (MonadState (get))
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans.Accum as M
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as A
import qualified Data.Either as DE
import Data.Functor as DF
import qualified Data.Map as M
import qualified Data.Maybe as A
import qualified Data.Maybe as DM
import qualified Data.Set as S
import qualified Data.Type.Bool as A
import Debug.Trace
import qualified Distribution.Simple as A
import Errors
import Foreign.C (throwErrno)
import GHC.Base (undefined)
import Grammar.AbsLatte (HasPosition (hasPosition))
import qualified Grammar.AbsLatte as A
import qualified Grammar.AbsLatte as E
import Grammar.PrintLatte
import qualified Text.PrettyPrint as A
import Utils
import Prelude as P

-- Variables (holds localVariables of any type).
-- Functions (holds function declarations, you can't assign to such functions. This map doesn't have info about
--  functions that are lambdas, localVariables or are function parameters).
-- Levels (holds info about level on which function or variable was declared - useful to check whether there are two declarations on the same block).
data Env = Env
  { localVariables :: M.Map A.UIdent A.Type,
    variableLevels :: M.Map A.UIdent Int,
    functions :: M.Map A.UIdent A.Type,
    level :: Int,
    functionRetType :: A.Type,
    currentClass :: Maybe A.UIdent,
    classes :: M.Map A.UIdent ClassType
  }
  deriving (Show)

data ClassType = ClassType
  { cAttrs :: M.Map A.UIdent A.Type,
    cFuncs :: M.Map A.UIdent A.Type,
    baseClass :: Maybe A.UIdent,
    classPosition :: A.BNFC'Position,
    cName :: A.UIdent
  }
  deriving (Show)

type ExprTEval a = ReaderT Env (ExceptT String Identity) a

runExprTEval :: Env -> ExprTEval a -> Either String a
runExprTEval env e = runIdentity (runExceptT (runReaderT e env))

typeOfExpr :: A.Expr -> ExprTEval A.Type
typeOfExpr (A.ELitInt pos _) = return $ A.TInt pos
typeOfExpr (A.ELitTrue pos) = return $ A.TBool pos
typeOfExpr (A.ELitFalse pos) = return $ A.TBool pos
typeOfExpr (A.EString pos _) = return $ A.TStr pos
typeOfExpr (A.Not pos e) = do
  typeOfExpr e >>= checkForTypeExact A.TBool pos
  return $ A.TBool pos
typeOfExpr (A.Neg pos e) = do
  typeOfExpr e >>= checkForTypeExact A.TInt pos
  return $ A.TInt pos
typeOfExpr (A.EAnd pos e1 e2) = typeOfBinOp A.TBool A.TBool pos e1 e2
typeOfExpr (A.EOr pos e1 e2) = typeOfBinOp A.TBool A.TBool pos e1 e2
typeOfExpr (A.EAdd pos e1 (A.Plus _) e2) = do
  t <- typeOfExpr e1
  case t of
    A.TStr _ -> typeOfBinOp A.TStr A.TStr pos e1 e2
    A.TInt _ -> typeOfBinOp A.TInt A.TInt pos e1 e2
    _ -> throwError $ showPosition (A.hasPosition e1) ++ unexpectedTypeMessage t ++ expectedTypeMessage (A.TStr pos) ++ " or " ++ printTree (A.TInt pos)
typeOfExpr (A.EAdd pos e1 _ e2) = typeOfBinOp A.TInt A.TInt pos e1 e2
typeOfExpr (A.EMul pos e1 _ e2) = typeOfBinOp A.TInt A.TInt pos e1 e2
typeOfExpr (A.ECastNull pos ident) = do
  env <- ask
  case M.lookup ident $ classes env of
    Nothing -> throwError $ noClassOfName pos ident
    Just _ -> return $ A.TClass pos ident
typeOfExpr (A.ESelf pos) = do
  env <- ask
  c <- maybeToError (currentClass env) (selfUsedOutsideOfClass pos)
  return $ A.TClass pos c
typeOfExpr (A.ERel pos e1 relOp e2) = do
  t1 <- typeOfExpr e1
  t2 <- typeOfExpr e2
  env <- ask
  assertM (equalTypesOrSecondIsSubtype env t1 t2 || equalTypesOrSecondIsSubtype env t2 t1) $ comparingValuesOfDifferentType pos t1 t2
  checkComparable relOp t1
  return $ A.TBool pos
  where
    checkComparableEq :: A.Type -> Bool
    checkComparableEq A.TFun {} = False
    checkComparableEq (A.TVoid _) = False
    checkComparableEq _ = True

    checkComparableLe :: A.Type -> Bool
    checkComparableLe (A.TInt _) = True
    checkComparableLe _ = False

    checkComparableHelp :: A.RelOp -> A.Type -> Bool
    checkComparableHelp (A.EQU _) t = checkComparableEq t
    checkComparableHelp (A.NE _) t = checkComparableEq t
    checkComparableHelp _ t = checkComparableLe t

    checkComparable :: A.RelOp -> A.Type -> ExprTEval ()
    checkComparable r t = assertM (checkComparableHelp r t) (typeNotComparable (A.hasPosition r) t r)
typeOfExpr (A.ENewObject pos t) = do
  env <- ask
  isValidClass t env
  where
    isValidClass :: MonadError String m => A.Type -> Env -> m A.Type
    isValidClass (A.TClass ppos ident) env = maybeToError (M.lookup ident (classes env)) (noClassOfName ppos ident) >> return t
    isValidClass tt _ = throwError $ showPosition pos ++ "cannot use new operator with type " ++ printTree tt
typeOfExpr (E.EVar pos ident) = do
  -- First  check local environment
  -- Then the classes
  env <- ask
  case M.lookup ident (localVariables env) of
    Nothing -> do
      currClassName <- maybeToError (currentClass env) (undefinedReferenceMessage ident pos)
      maybeToError (getSomethingFromClassOrSuperClasses (M.lookup ident . cAttrs) currClassName env) (undefinedReferenceMessage ident pos)
    Just ty -> return ty
typeOfExpr (A.EApp pos uident exprs) = do
  env <- ask
  f <- case currentClass env of
    Nothing -> maybeToError (M.lookup uident (functions env)) (undefinedReferenceMessage uident pos)
    Just currClassName -> do
      case getSomethingFromClassOrSuperClasses (M.lookup uident . cAttrs) currClassName env of
        Just x -> return x
        Nothing -> maybeToError (M.lookup uident (functions env)) (undefinedReferenceMessage uident pos)

  handleFunction pos f exprs
typeOfExpr (A.EMember pos expr member) = do
  env <- ask
  t <- typeOfExpr expr
  case t of
    A.TClass _ className -> maybeToError (getSomethingFromClassOrSuperClasses (M.lookup member . cAttrs) className env) (undefinedMember pos className member)
    _ -> throwError $ showPosition pos ++ "cannot get a member of expression that is not a class"
typeOfExpr (A.EMemberCall pos expr member exprs) = do
  env <- ask
  t <- typeOfExpr expr
  case t of
    A.TClass _ className -> do
      f <- maybeToError (getSomethingFromClassOrSuperClasses (M.lookup member . cFuncs) className env) (undefinedMember pos className member)
      handleFunction pos f exprs
    _ -> throwError $ showPosition pos ++ "cannot get a member of expression that is not a class"

-- Checks if function application is performed correctly. If so, returns its return type.
handleFunction :: A.BNFC'Position -> A.Type -> [A.Expr] -> ExprTEval A.Type
handleFunction pos (A.TFun _ retType params) args =
  checkArgsCorrectness pos params args >> return retType
handleFunction _ _ _ = undefined

checkArgsCorrectness ::
  A.BNFC'Position -> [A.Type] -> [A.Expr] -> ExprTEval ()
checkArgsCorrectness pos params args = do
  assertM (P.length params == P.length args) $
    showPosition pos
      ++ "function expected "
      ++ show (P.length params)
      ++ " argument(s), received "
      ++ show (P.length args)
  zipWithM_ checkArgCorrectness params args

checkArgCorrectness :: A.Type -> A.Expr -> ExprTEval ()
checkArgCorrectness param arg = do
  env <- ask
  argType <- typeOfExpr arg
  assertM (equalTypesOrSecondIsSubtype env param argType) (errorMessageWrongType (A.hasPosition arg) argType param)

typeOfBinOp ::
  (A.BNFC'Position -> A.Type) ->
  (A.BNFC'Position -> A.Type) ->
  A.BNFC'Position ->
  A.Expr ->
  A.Expr ->
  ExprTEval A.Type
typeOfBinOp typeConstructor retTypeConstructor pos e1 e2 = do
  typeOfExpr e1 >>= checkForTypeExact typeConstructor (A.hasPosition e1)
  typeOfExpr e2 >>= checkForTypeExact typeConstructor (A.hasPosition e2)
  return $ retTypeConstructor pos

checkForTypeExact ::
  MonadError String m =>
  (A.BNFC'Position -> A.Type) ->
  A.BNFC'Position ->
  A.Type ->
  m ()
checkForTypeExact typeConstructor pos t =
  assertM
    (isTypeExact t typeConstructor)
    (errorMessageWrongType pos t $ typeConstructor pos)

checkExpressionIsLValue :: A.Expr -> StmtTEval ()
checkExpressionIsLValue (A.EVar _ _) = return ()
checkExpressionIsLValue A.EMember {} = return ()
checkExpressionIsLValue e = throwError $ errorMessageNotAnLValue (A.hasPosition e) e

-- Statement typechecker
type StmtTEval a = StateT Env (ExceptT String Identity) a

incrementBlockLevel :: Env -> Env
incrementBlockLevel env = env {level = (+ 1) $ level env}

typeStmt :: A.Stmt -> StmtTEval ()
typeStmt (A.SEmpty _) = return ()
typeStmt (A.SCond _ expr stmt) = do
  checkExpressionTypeEqualExact (A.TBool A.BNFC'NoPosition) expr
  env <- get
  typeStmt $ A.SBStmt (A.hasPosition stmt) $ A.SBlock (A.hasPosition stmt) [stmt]
  put env
typeStmt (A.SCondElse _ expr b1 b2) = do
  checkExpressionTypeEqualExact (A.TBool A.BNFC'NoPosition) expr
  env <- get
  typeStmt $ A.SBStmt (A.hasPosition b1) $ A.SBlock (A.hasPosition b1) [b1]
  put env
  typeStmt $ A.SBStmt (A.hasPosition b2) $ A.SBlock (A.hasPosition b2) [b2]
  put env
typeStmt (A.SExp _ expr) = do
  env <- get
  liftEither $ runExprTEval env (typeOfExpr expr)
  return ()
typeStmt (A.SWhile _ expr stmt) = do
  checkExpressionTypeEqualExact (A.TBool A.BNFC'NoPosition) expr
  env <- get
  typeStmt $ A.SBStmt (A.hasPosition stmt) $ A.SBlock (A.hasPosition stmt) [stmt]
  put env
typeStmt (A.SDecl _ t items) = do
  checkTypeCorrectUtil (A.hasPosition t) t
  mapM_ (addItemToEnv t) items
  where
    addItemToEnv :: A.Type -> A.Item -> StmtTEval ()
    addItemToEnv itemType (A.SNoInit pos ident) = do
      -- TODO check for shadowing
      env <- get
      checkVariableLevel pos ident
      put $
        env
          { localVariables = M.insert ident itemType (localVariables env),
            variableLevels = M.insert ident (level env) (variableLevels env)
          }
      return ()
    addItemToEnv itemType (A.SInit pos ident expr) = do
      checkVariableLevel pos ident
      env <- get
      put $
        env
          { localVariables = M.insert ident t (localVariables env),
            variableLevels = M.insert ident (level env) (variableLevels env)
          }
      checkExpressionTypeEqualOrSubType itemType expr
      return ()
typeStmt (A.SRet _ expr) = do
  env <- get
  checkExpressionTypeEqualOrSubType (functionRetType env) expr
  return ()
typeStmt (A.SVRet pos) = do
  funcT <- gets functionRetType
  let voidType = A.TVoid pos
  assertM (typesEq voidType funcT) $ errorMessageWrongType pos voidType funcT
typeStmt (A.SAss _ lExpr rExpr) = do
  checkExpressionIsLValue lExpr
  checkExpressionsEqualTypeOrSecondIsSubType lExpr rExpr
typeStmt (A.SIncr pos lExpr) = do
  checkExpressionIsLValue lExpr
  checkExpressionTypeEqualExact (A.TInt pos) lExpr
typeStmt (A.SDecr pos lExpr) = do
  checkExpressionIsLValue lExpr
  checkExpressionTypeEqualExact (A.TInt pos) lExpr
typeStmt (A.SBStmt _ (A.SBlock _ stmts)) = do
  env <- get
  put $ incrementBlockLevel env
  mapM_ typeStmt stmts
  put env
  return ()

isTypeExact :: A.Type -> (A.BNFC'Position -> A.Type) -> Bool
isTypeExact t1 t2 = typesEq t1 $ t2 A.BNFC'NoPosition

isClassOrSuperClass :: Env -> A.UIdent -> A.UIdent -> Bool
isClassOrSuperClass env className bClassName = case getSomethingFromClassOrSuperClasses hasBaseClassName className env of
  Just _ -> True
  _ -> False
  where
    hasBaseClassName x = if cName x == bClassName then return (Just ()) else Nothing

equalTypesOrSecondIsSubtype :: Env -> A.Type -> A.Type -> Bool
equalTypesOrSecondIsSubtype env (A.TClass _ bClassName) (A.TClass _ t) = isClassOrSuperClass env t bClassName
equalTypesOrSecondIsSubtype _ t1 t2 = typesEq t1 t2

-- Check if variable ident can be declared at the given level.
checkVariableLevel :: A.BNFC'Position -> A.UIdent -> StmtTEval ()
checkVariableLevel pos ident = do
  env <- get
  let lvl = level env
  case M.lookup ident (variableLevels env) of
    Nothing -> return ()
    Just varLevel ->
      assertM (varLevel /= lvl) $
        showPosition pos
          ++ "variable "
          ++ printTree ident
          ++ " was already declared at this level"

-- Checks if type is a correct class, Int, string or bool
checkTypeCorrectUtil :: A.BNFC'Position -> A.Type -> StmtTEval ()
checkTypeCorrectUtil pos (A.TClass _ ident) = do
  env <- get
  case M.lookup ident (classes env) of
    Just _ -> return ()
    Nothing ->
      throwError $
        showPosition pos
          ++ "cannot find class "
          ++ printTree ident
checkTypeCorrectUtil pos (A.TVoid _) = throwError $ showPosition pos ++ "cannot use type 'void' in this place"
checkTypeCorrectUtil pos A.TFun {} = throwError $ showPosition pos ++ "cannot use type 'function' in this place"
checkTypeCorrectUtil _ _ = return ()

checkExpressionsEqualTypeOrSecondIsSubType :: A.Expr -> A.Expr -> StmtTEval ()
checkExpressionsEqualTypeOrSecondIsSubType e1 e2 = do
  env <- get
  expr1Type <- liftEither $ runExprTEval env (typeOfExpr e1)
  expr2Type <- liftEither $ runExprTEval env (typeOfExpr e2)
  assertM (equalTypesOrSecondIsSubtype env expr1Type expr2Type) $
    errorMessageWrongType (A.hasPosition e1) expr2Type expr1Type
  return ()

checkExpressionTypeEqualExact :: A.Type -> A.Expr -> StmtTEval ()
checkExpressionTypeEqualExact t expr = do
  env <- get
  exprType <- liftEither $ runExprTEval env (typeOfExpr expr)
  assertM (typesEq exprType t) $
    errorMessageWrongType (A.hasPosition expr) exprType t
  return ()

checkExpressionTypeEqualOrSubType :: A.Type -> A.Expr -> StmtTEval ()
checkExpressionTypeEqualOrSubType t expr = do
  env <- get
  exprType <- liftEither $ runExprTEval env (typeOfExpr expr)
  assertM (equalTypesOrSecondIsSubtype env t exprType) $
    errorMessageWrongType (A.hasPosition expr) exprType t
  return ()

checkIfMainDef :: A.TopDef -> Bool
checkIfMainDef (A.TopFuncDef _ (A.FunDefT _ retType ident args _)) =
  ident == A.UIdent "main" && isTypeExact retType A.TInt && null args
checkIfMainDef _ = False

getArgType :: A.Arg -> A.Type
getArgType (A.ArgT _ t _) = t

addArgToEnv :: MonadError String m => Int -> Env -> A.Arg -> m Env
addArgToEnv newLevel env (A.ArgT pos t ident) = do
  -- TODO add here a warning if we shadow a variable / class variable
  liftEither $ runStmtTEval env $ checkVariableLevel pos ident
  return
    env
      { localVariables = M.insert ident t (localVariables env),
        variableLevels = M.insert ident newLevel (variableLevels env)
      }

typeProgram :: A.Program -> StmtTEval ()
typeProgram (A.ProgramT _ topdefs) = do
  assertM (P.any checkIfMainDef topdefs) "No main function"
  mapM_ addTopDefToEnv topdefs
  env <- get
  liftEither $ checkForCycles (classes env)
  mapM_ typeTopDef topdefs

checkForCycles :: M.Map A.UIdent ClassType -> Either String ()
checkForCycles graph = foldM_ checkForCyclesHelp S.empty (M.keys graph)
  where
    checkForCyclesHelp :: S.Set A.UIdent -> A.UIdent -> Either String (S.Set A.UIdent)
    checkForCyclesHelp previousVisited currClass = checkForCyclesHelpRec previousVisited S.empty currClass

    checkForCyclesHelpRec :: S.Set A.UIdent -> S.Set A.UIdent -> A.UIdent -> Either String (S.Set A.UIdent)
    checkForCyclesHelpRec previousVisited visited currClass = do
      let currClassT = DM.fromJust $ M.lookup currClass graph
      assertM (S.notMember currClass visited) $ cyclicInheritance $ classPosition currClassT
      if S.member currClass previousVisited
        then return (S.union visited previousVisited)
        else case currClassT of
          (ClassType _ _ (Just bClass) _ _) -> do
            let newVisited = S.insert currClass visited
            checkForCyclesHelpRec previousVisited newVisited bClass
          _ -> return (S.union visited previousVisited)

-- This only adds functions to the environment
addTopDefToEnv :: A.TopDef -> StmtTEval ()
addTopDefToEnv (A.TopFuncDef _ (A.FunDefT pos retType funName args _)) = do
  env <- get
  when (M.member funName (functions env)) $ throwError $ functionHasAlreadyBeenDeclared pos funName
  let newFunctions =
        M.insert
          funName
          (A.TFun pos retType (P.map getArgType args))
          (functions env)
  put $ env {functions = newFunctions}
addTopDefToEnv (A.TopClassDef pos classDef) = do
  let className = getClassName classDef
  env <- get
  when
    (M.member className (classes env))
    $ throwError
    $ classHasAlreadyBeenDeclared pos className
  classType <- createClassTypeFromClassDef classDef
  let newClasses =
        M.insert
          className
          classType
          (classes env)
  put $ env {classes = newClasses}

calculateExprCompileTime :: A.Expr -> Maybe Bool
calculateExprCompileTime (A.ELitFalse _) = Just False
calculateExprCompileTime (A.ELitTrue _) = Just True
calculateExprCompileTime _ = Nothing

stmtReturnsValue :: MonadError String m => A.Stmt -> m Bool
stmtReturnsValue (A.SRet _ _) = return True
stmtReturnsValue (A.SVRet _) = return True
stmtReturnsValue (A.SWhile _ expr stmt) = case calculateExprCompileTime expr of
  Just True -> stmtReturnsValue stmt
  _ -> return False
stmtReturnsValue (A.SCond _ expr stmt) = case calculateExprCompileTime expr of
  Just True -> stmtReturnsValue stmt
  _ -> return False
stmtReturnsValue (A.SCondElse _ expr stmt1 stmt2) = case calculateExprCompileTime expr of
  Just True -> stmtReturnsValue stmt1
  Just False -> stmtReturnsValue stmt2
  _ -> do
    r1 <- stmtReturnsValue stmt1
    r2 <- stmtReturnsValue stmt2
    return $ r1 && r2
stmtReturnsValue (A.SBStmt _ (A.SBlock _ stmts)) =
  mapM stmtReturnsValue stmts <&> or
stmtReturnsValue _ = return False

checkReturn :: A.Type -> A.Stmt -> StmtTEval ()
checkReturn (A.TVoid _) _ = return ()
checkReturn _ stmt = do
  x <- stmtReturnsValue stmt
  if x
    then return ()
    else throwError $ functionDoesntReturnValue $ A.hasPosition stmt

getSomethingFromClassOrSuperClasses :: (ClassType -> Maybe a) -> A.UIdent -> Env -> Maybe a
getSomethingFromClassOrSuperClasses method className env =
  let cType = DM.fromJust $ M.lookup className (classes env)
   in case method cType of
        Just x -> return x
        Nothing -> do
          superClass <- baseClass cType
          getSomethingFromClassOrSuperClasses method superClass env

-- Doesn't change the environment
validateMethod :: A.Type -> [A.Arg] -> A.Block -> StmtTEval ()
validateMethod retType args block = do
  env <- get
  let incrementedLevel = level env + 1
  envWithAddedParams <- foldM (addArgToEnv incrementedLevel) (env {level = incrementedLevel}) args
  put $
    env
      { variableLevels = variableLevels envWithAddedParams,
        localVariables = localVariables envWithAddedParams,
        functionRetType = retType,
        level = incrementedLevel
      }
  typeStmt $ A.SBStmt (A.hasPosition block) block
  checkReturn retType $ A.SBStmt (A.hasPosition block) block
  put env

typeTopDef :: A.TopDef -> StmtTEval ()
typeTopDef (A.TopFuncDef _ (A.FunDefT _ retType _ args block)) = validateMethod retType args block
typeTopDef (A.TopClassDef _ (A.ClassDefT _ uident classMembers)) = typeClassHelp uident classMembers
typeTopDef (A.TopClassDef _ (A.ClassExtDefT _ uident _ classMembers)) = typeClassHelp uident classMembers

typeClassHelp :: A.UIdent -> [A.ClassMember] -> StmtTEval ()
typeClassHelp uident classMembers = do
  env <- get
  let cType = DM.fromJust $ M.lookup uident (classes env)
  liftEither $ mapM_ (validateClassAttribute cType env) $ M.keys $ cAttrs cType
  mapM_ (validateClassMethod cType) $ classMembers
  where
    validateClassAttribute :: ClassType -> Env -> A.UIdent -> Either String ()
    validateClassAttribute cType env attribute = case superClassHasAttribute of
      Just _ -> throwError $ attributeAlreadyDeclaredForThisClassOrSuprclass (A.hasPosition $ DM.fromJust $ M.lookup attribute $ cAttrs cType) attribute
      Nothing -> return ()
      where
        superClassHasAttribute :: Maybe A.Type
        superClassHasAttribute = do
          let f = M.lookup attribute . cAttrs
          x <- baseClass cType
          getSomethingFromClassOrSuperClasses f x env

    validateClassMethod :: ClassType -> A.ClassMember -> StmtTEval ()
    validateClassMethod cType (A.ClassMethodT _ (A.FunDefT pos retType name args block)) = do
      env <- get
      let funcType = A.TFun pos retType $ P.map getArgType args
      _ <- case superClassHasMethod env of
        Just func -> unless (typesEq func funcType) $ throwError (functionWithDifferentTypeAlreadyDeclaredForThisClass pos name)
        _ -> return ()

      let envWithAddedClass =
            env
              { currentClass = Just $ cName cType
              }
      put envWithAddedClass
      validateMethod retType args block
      put env
      where
        f :: ClassType -> Maybe A.Type
        f x = do
          M.lookup name (cFuncs x)

        superClassHasMethod :: Env -> Maybe A.Type
        superClassHasMethod env = do
          x <- baseClass cType
          getSomethingFromClassOrSuperClasses f x env
    validateClassMethod _ _ = return ()

getClassName :: A.ClassDef -> A.UIdent
getClassName (A.ClassDefT _ className _) = className
getClassName (A.ClassExtDefT _ className _ _) = className

createClassTypeFromClassDef :: A.ClassDef -> StmtTEval ClassType
createClassTypeFromClassDef (A.ClassDefT pos className classMembers) = createClassTypeFromClassMembers pos className DM.Nothing classMembers
createClassTypeFromClassDef (A.ClassExtDefT pos className baseClassName classMembers) = createClassTypeFromClassMembers pos className (DM.Just baseClassName) classMembers

createClassTypeFromClassMembers :: A.BNFC'Position -> A.UIdent -> Maybe A.UIdent -> [A.ClassMember] -> StmtTEval ClassType
createClassTypeFromClassMembers pos className bClass classMembers = do
  let acc =
        ClassType
          { cAttrs = M.empty,
            cFuncs = M.empty,
            baseClass = bClass,
            classPosition = pos,
            cName = className
          }
  foldM createClassTypeFromClassMembersHelp acc classMembers
  where
    createClassTypeFromClassMembersHelp :: ClassType -> A.ClassMember -> StmtTEval ClassType
    createClassTypeFromClassMembersHelp accClassType (A.ClassFieldT _ t ident) = do
      case M.lookup ident (cAttrs accClassType) of
        Just _ -> throwError $ attributeAlreadyDeclaredForThisClass pos ident
        Nothing -> do
          let newCAttrs = M.insert ident t (cAttrs accClassType)
          return
            accClassType
              { cAttrs = newCAttrs
              }
    createClassTypeFromClassMembersHelp accClassType (A.ClassMethodT _ (A.FunDefT _ retType ident args _)) = do
      case M.lookup ident (cFuncs accClassType) of
        Just _ -> throwError $ functionAlreadyDeclaredForThisClass pos ident
        Nothing -> do
          let fType = A.TFun pos retType (P.map getArgType args)
          let newCFuncs = M.insert ident fType (cFuncs accClassType)
          return
            accClassType
              { cFuncs = newCFuncs
              }

runStmtTEval :: Env -> StmtTEval a -> Either String (a, Env)
runStmtTEval env e = runIdentity (runExceptT (runStateT e env))

addFunctions :: Env -> Env
addFunctions e = snd $ DE.fromRight ((), initEnv) $ runStmtTEval e x
  where
    x = do
      mapM_ addTopDefToEnv builtInFunctions

initEnv :: Env
initEnv =
  addFunctions $
    Env
      { localVariables = M.empty,
        variableLevels = M.empty,
        functions = M.empty,
        level = 0,
        functionRetType = A.TVoid A.BNFC'NoPosition, -- won't be used anyway.,
        currentClass = Nothing,
        classes = M.empty
      }

runSemanticAnalysis :: A.Program -> Either String ((), Env)
runSemanticAnalysis p = runStmtTEval initEnv (typeProgram p)
