{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalysis where

import Control.Monad (foldM, when)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS (MonadState (get))
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans.Accum as M
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Maybe as A
import qualified Data.Either as DE
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Set as S
import qualified Distribution.Simple as A
import Errors
import Foreign.C (throwErrno)
import GHC.Base (undefined)
import qualified Grammar.AbsLatte as A
import Grammar.PrintLatte
import System.Posix (blockSignals)
import System.Posix.Internals (fdType)
import System.Process (CreateProcess (env))
import Text.XHtml (base)
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
  typeOfExpr e >>= checkForType A.TBool pos
  return $ A.TBool pos
typeOfExpr (A.EAnd pos e1 e2) = typeOfBinOp A.TBool A.TBool pos e1 e2
typeOfExpr (A.EOr pos e1 e2) = typeOfBinOp A.TBool A.TBool pos e1 e2
typeOfExpr (A.EAdd pos e1 _ e2) = typeOfBinOp A.TInt A.TInt pos e1 e2
typeOfExpr (A.EMul pos e1 _ e2) = typeOfBinOp A.TInt A.TInt pos e1 e2
typeOfExpr (A.ECastNull pos ident) = do
  env <- ask
  case M.lookup ident $ classes env of
    Nothing -> throwError $ noClassOfName pos ident
    Just _ -> return $ A.TClass pos ident
typeOfExpr (A.ESelf pos) = do
  env <- ask
  case currentClass env of
    Just c -> return $ A.TClass pos c
    Nothing -> throwError $ selfUsedOutsideOfClass pos
typeOfExpr (A.ERel pos e1 _ e2) = do
  t1 <- typeOfExpr e1
  t2 <- typeOfExpr e2
  assertM (typesEq t1 t2) $ comparingValuesOfDifferentType pos t1 t2
  checkComparable t1
  return $ A.TBool pos
  where
    checkComparable :: A.Type -> ExprTEval ()
    checkComparable f@A.TFun {} =
      throwError $ typeNotComparable pos f
    checkComparable c@(A.TClass _ _) =
      throwError $ typeNotComparable pos c
    checkComparable c@(A.TVoid _) =
      throwError $ typeNotComparable pos c
    checkComparable _ = return ()
typeOfExpr _ = undefined

-- Type contains also information about position of the expression that has the type returned.
-- typeOfExpr :: A.Expr -> ExprTEval A.Type
-- typeOfExpr (A.EVar pos ident) = do
--   env <- ask
--   case M.lookup ident $ localVariables env of
--     Nothing ->
--       case M.lookup ident $ functions env of
--         Nothing -> throwError $ (undefinedReferenceMessage ident pos) ++ "\n"
--         Just t -> return t
--     Just t -> return t
-- -- Literals.
-- typeOfExpr (A.ELitTrue pos) = return $ A.TBool pos
-- typeOfExpr (A.ELitFalse pos) = return $ A.TBool pos
-- typeOfExpr (A.ELitInt pos _) = return $ A.TInt pos
-- typeOfExpr (A.EString pos _) = return $ A.TStr pos
-- -- Binary operator expressions.
-- typeOfExpr (A.EAnd pos e1 e2) = typeOfBinOp A.Bool A.Bool pos e1 e2
-- typeOfExpr (A.EOr pos e1 e2) = typeOfBinOp A.Bool A.Bool pos e1 e2
-- typeOfExpr (A.EAdd pos e1 _ e2) = typeOfBinOp A.Int A.Int pos e1 e2
-- typeOfExpr (A.EMul pos e1 _ e2) = typeOfBinOp A.Int A.Int pos e1 e2
-- typeOfExpr (A.ERel pos e1 _ e2) = do
--   t1 <- typeOfExpr e1
--   t2 <- typeOfExpr e2
--   assertM (typesEq t1 t2) $
--     showPosition pos
--       ++ "comparing values of diferent type, "
--       ++ printTree t1
--       ++ " and "
--       ++ printTree t2
--   checkComparable t1
--   return $ A.Bool pos
--   where
--     checkComparable :: A.Type -> ExprTEval ()
--     checkComparable f@(A.TFun _ _ _) =
--       throwError $ showPosition pos ++ printTree f ++ " type is not comparable"
--     checkComparable _ = return ()
-- -- Unary operator expressions.
-- typeOfExpr (A.Not pos e) = do
--   typeOfExpr e >>= checkForType A.Bool pos
--   return $ A.Bool pos
-- typeOfExpr (A.Neg pos e) = do
--   typeOfExpr e >>= checkForType A.Int pos
--   return $ A.Int pos
-- typeOfExpr (A.ETuple pos l) =
--   -- foldr (liftM2 (:)) (pure []) changes list of monads to monad of list.
--   -- http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--   do
--     listOfTypes <- P.foldr (liftM2 (:)) (pure []) $ typeOfExpr <$> l
--     return $ A.Tuple pos listOfTypes
-- typeOfExpr (A.ELambda pos (A.Lambda _ arguments retType body)) = do
--   env <- ask
--   let incrementedLevelEnv = incrementBlockLevel env
--   envWithAddedParams <-
--     foldM
--       (addArgToEnv (level incrementedLevelEnv))
--       incrementedLevelEnv
--       arguments
--   let blockEnv =
--         incrementedLevelEnv
--           { functionRetType = retType,
--             variableLevels = variableLevels envWithAddedParams,
--             localVariables = localVariables envWithAddedParams
--           }
--   liftEither $
--     runStmtTEval blockEnv $
--       typeStmt $
--         A.BStmt (A.hasPosition body) body
--   return $ Function pos retType $ getArgType <$> arguments
-- typeOfExpr (A.EApp pos callee args) =
--   case callee of
--     A.LambdaCallee p l -> do
--       t <- typeOfExpr (A.ELambda p l)
--       handleFunction pos t args
--     A.UIdentCallee p ident -> do
--       t <- typeOfExpr (A.EVar p ident)
--       -- Evar
--       -- 1. Will check if there exists variable of given iden, if so it returs its type.
--       -- 2. If not, checks if there exists function of given iden, if so, it returns its type.
--       -- What is left to handle is the case when both function and variable of given iden exist and
--       -- variable was not of a suitable type.
--       -- Maybe right now I should not do it.
--       handleFunction pos t args

-- ~ catchError
-- ~ (handleFunction pos t args)
-- ~ (\_ -> do
-- ~ env <- ask
-- ~ case M.lookup ident (functions env) of
-- ~ Just t -> handleFunction pos t args
-- ~ Nothing ->
-- ~ throwError $
-- ~ showPosition pos ++ "no function " ++ printTree ident ++ " found")

-- Checks if function application is performed correctly. If so, returns its return type.
-- handleFunction :: A.BNFC'Position -> A.Type -> [A.Expr] -> ExprTEval A.Type
-- handleFunction pos f args =
--   case f of
--     A.Function _ retType params ->
--       (checkArgsCorrectness pos params args) >>= (\_ -> return retType)
--     _ -> throwError $ notAFunctionMessage pos f

-- checkArgsCorrectness ::
--   A.BNFC'Position -> [A.ArgType] -> [A.Expr] -> ExprTEval ()
-- checkArgsCorrectness pos params args = do
--   assertM (P.length params == P.length args) $
--     showPosition pos
--       ++ "function expected "
--       ++ show (P.length params)
--       ++ " argument(s), received "
--       ++ show (P.length args)
--   zipWithM checkArgCorrectness args params
--   return ()

-- checkArgCorrectness :: A.Expr -> A.ArgType -> ExprTEval ()
-- checkArgCorrectness arg param =
--   case param of
--     A.ArgRef _ _ ->
--       case arg of
--         A.EVar _ ident ->
--           -- We evaluate varType by hand because we don't want functions environment to be checked.
--           do
--             varType <-
--               do
--                 env <- ask
--                 case M.lookup ident (localVariables env) of
--                   Just t -> return t
--                   Nothing ->
--                     throwError $ errorWrongArgumentPassedByReference arg param
--             let paramType = (getTypeFromArgType param)
--             assertM
--               (typesEq varType paramType)
--               (errorMessageWrongType (A.hasPosition arg) varType paramType)
--         _ -> throwError $ errorWrongArgumentPassedByReference arg param
--     _ -> do
--       argType <- typeOfExpr arg
--       let paramType = (getTypeFromArgType param)
--       assertM
--         (typesEq argType paramType)
--         (errorMessageWrongType (A.hasPosition arg) argType paramType)

-- getArgType :: A.Arg -> A.ArgType
-- getArgType (A.Arg _ t _) = t

-- getTypeFromArgType :: A.ArgType -> A.Type
-- getTypeFromArgType (A.ArgRef _ t) = t
-- getTypeFromArgType (A.ArgT _ t) = t

typeOfBinOp ::
  (A.BNFC'Position -> A.Type) ->
  (A.BNFC'Position -> A.Type) ->
  A.BNFC'Position ->
  A.Expr ->
  A.Expr ->
  ExprTEval A.Type
typeOfBinOp typeConstructor retTypeConstructor pos e1 e2 = do
  typeOfExpr e1 >>= checkForType typeConstructor (A.hasPosition e1)
  typeOfExpr e2 >>= checkForType typeConstructor (A.hasPosition e2)
  return $ retTypeConstructor pos

checkForType ::
  MonadError String m =>
  (A.BNFC'Position -> A.Type) ->
  A.BNFC'Position ->
  A.Type ->
  m ()
checkForType typeConstructor pos t =
  assertM
    (isType t typeConstructor)
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
typeStmt (A.SCond _ expr block) = do
  checkExpressionType (A.TBool A.BNFC'NoPosition) expr
  env <- get
  typeStmt block
  put env
typeStmt (A.SCondElse _ expr b1 b2) = do
  checkExpressionType (A.TBool A.BNFC'NoPosition) expr
  env <- get
  typeStmt b1
  put env >> typeStmt b2
  put env
typeStmt (A.SExp _ expr) = do
  env <- get
  liftEither $ runExprTEval env (typeOfExpr expr)
  return ()
typeStmt (A.SWhile _ expr stmt) = do
  checkExpressionType (A.TBool A.BNFC'NoPosition) expr
  env <- get
  typeStmt stmt
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
      checkExpressionType itemType expr
      return ()
typeStmt (A.SRet _ expr) = do
  env <- get
  checkExpressionType (functionRetType env) expr
  return ()
typeStmt (A.SVRet pos) = do
  funcT <- gets functionRetType
  let voidType = A.TVoid pos
  assertM (typesEq voidType funcT) $ errorMessageWrongType pos voidType funcT
typeStmt (A.SAss _ lExpr rExpr) = do
  checkExpressionIsLValue lExpr
  checkExpressionsEqualType lExpr rExpr
typeStmt (A.SIncr pos lExpr) = do
  checkExpressionIsLValue lExpr
  checkExpressionType (A.TInt pos) lExpr
typeStmt (A.SDecr pos lExpr) = do
  checkExpressionIsLValue lExpr
  checkExpressionType (A.TInt pos) lExpr
typeStmt (A.SBStmt _ (A.SBlock _ stmts)) = do
  env <- get
  put $ incrementBlockLevel env
  mapM_ typeStmt stmts
  put env
  return ()

-- typeStmt (A.Ass pos ident expr) = do
--   vars <- liftM localVariables get
--   case M.lookup ident vars of
--     Nothing ->
--       throwError $
--         showPosition pos ++ "attempting to assign to an undeclared variable"
--     Just t -> checkExpressionType t expr
-- typeStmt (A.TupleAss pos tupleIdents expr) = do
--   env <- get
--   exprType <- liftEither $ runExprTEval env (typeOfExpr expr)
--   case exprType of
--     A.Tuple _ types -> do
--       assertM (P.length types == P.length tupleIdents) $
--         showPosition pos
--           ++ "error unpacking tuple, argument numbers don't match"
--       zipWithM handleTupleIdent tupleIdents types
--       return ()
--     t ->
--       throwError $
--         showPosition pos
--           ++ "attempting to unpack tuple with expression of type "
--           ++ printTree t
--           ++ ", that is not a tuple"
--   where
--     handleTupleIdent :: A.TupleIdent -> A.Type -> StmtTEval ()
--     handleTupleIdent (A.TupleNoIdent _) _ = return ()
--     handleTupleIdent (A.TupleIdent p ident) t =
--       -- Same code as during assignment, only don't check the type of expression
--       -- (as we know the type from typing the tuple).
--       do
--         vars <- liftM localVariables get
--         case M.lookup ident vars of
--           Nothing ->
--             throwError $
--               showPosition p ++ "attempting to assign to an undeclared variable"
--           Just varType ->
--             assertM (typesEq varType t) $
--               showPosition p
--                 ++ "attempting to assign expression of type "
--                 ++ printTree t
--                 ++ " to a variable of type "
--                 ++ printTree varType
--     handleTupleIdent (A.TupleRec _ idents) t = do
--       case t of
--         A.Tuple _ types -> do
--           assertM (P.length types == P.length idents) $
--             showPosition pos
--               ++ "error unpacking tuple, argument numbers don't match"
--           zipWithM handleTupleIdent idents types
--           return ()
--         wrongType ->
--           throwError $
--             showPosition pos
--               ++ "attempting to unpack tuple with expression of type "
--               ++ printTree wrongType
--               ++ ", that is not a tuple"
-- typeStmt (BStmt _ (Block _ stmts)) = do
--   env <- get
--   put $ incrementBlockLevel env
--   mapM_ typeStmt stmts
--   put env
--   return ()

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

-- checkFunctionLevel :: A.BNFC'Position -> Ident -> StmtTEval ()
-- checkFunctionLevel pos ident = do
--   env <- get
--   let lvl = level env
--   case M.lookup ident (functionLevels env) of
--     Nothing -> return ()
--     Just varLevel ->
--       assertM (varLevel /= lvl) $
--         showPosition pos
--           ++ "function "
--           ++ printTree ident
--           ++ " was already declared at this level"

checkExpressionsEqualType :: A.Expr -> A.Expr -> StmtTEval ()
checkExpressionsEqualType e1 e2 = do
  env <- get
  expr1Type <- liftEither $ runExprTEval env (typeOfExpr e1)
  expr2Type <- liftEither $ runExprTEval env (typeOfExpr e2)
  assertM (typesEq expr2Type expr1Type) $
    errorMessageWrongType (A.hasPosition e1) expr2Type expr1Type
  return ()

checkExpressionType :: A.Type -> A.Expr -> StmtTEval ()
checkExpressionType t expr = do
  env <- get
  exprType <- liftEither $ runExprTEval env (typeOfExpr expr)
  assertM (typesEq exprType t) $
    errorMessageWrongType (A.hasPosition expr) exprType t
  return ()

checkIfMainDef :: A.TopDef -> Bool
checkIfMainDef (A.TopFuncDef _ (A.FunDefT _ retType ident args _)) =
  ident == A.UIdent "main" && isType retType A.TInt && null args
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
          (ClassType _ _ (Just baseClass) _ _) -> do
            let newVisited = S.insert currClass visited
            checkForCyclesHelpRec previousVisited newVisited baseClass
          _ -> return (S.union visited previousVisited)

-- This only adds functions to the environment
addTopDefToEnv :: A.TopDef -> StmtTEval ()
addTopDefToEnv (A.TopFuncDef _ (A.FunDefT pos retType funName args block)) = do
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

checkReturn :: A.Stmt -> StmtTEval ()
checkReturn _ = undefined

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
  checkReturn $ A.SBStmt (A.hasPosition block) block
  put env

typeTopDef :: A.TopDef -> StmtTEval ()
typeTopDef (A.TopFuncDef _ (A.FunDefT pos retType funName args block)) = validateMethod retType args block
typeTopDef (A.TopClassDef pos (A.ClassDefT _ uident classMembers)) = typeClassHelp uident classMembers
typeTopDef (A.TopClassDef pos (A.ClassExtDefT _ uident _ classMembers)) = typeClassHelp uident classMembers

typeClassHelp :: A.UIdent -> [A.ClassMember] -> StmtTEval ()
typeClassHelp uident classMembers = do
  env <- get
  let cType = DM.fromJust $ M.lookup uident (classes env)
  liftEither $ mapM_ (validateClassAttribute cType env) $ M.keys $ cAttrs cType
  undefined
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
      validateMethod retType args block
      put env
      where
        f :: ClassType -> Maybe A.Type
        f x = do
          let cFunType = A.TFun pos retType $ P.map getArgType args
          M.lookup name (cFuncs x)

        superClassHasMethod :: Env -> Maybe A.Type
        superClassHasMethod env = do
          x <- baseClass cType
          getSomethingFromClassOrSuperClasses f x env
    validateClassMethod _ _ = return ()

-- data ClassType = ClassType
--   { cAttrs :: M.Map A.UIdent A.Type,
--     cFuncs :: M.Map A.UIdent A.Type,
--     baseClass :: Maybe A.UIdent
--   }
--   deriving (Show)

-- data Env = Env
--   { localVariables :: M.Map A.UIdent A.Type,
--     variableLevels :: M.Map A.UIdent Int,
--     functions :: M.Map A.UIdent A.Type,
--     level :: Int,
--     functionRetType :: A.Type,
--     currentClass :: Maybe A.UIdent,
--     classes :: M.Map A.UIdent ClassType
--   }
--   deriving (Show)

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
    createClassTypeFromClassMembersHelp accClassType (A.ClassFieldT pos t ident) = do
      case M.lookup ident (cAttrs accClassType) of
        Just _ -> throwError $ attributeAlreadyDeclaredForThisClass pos ident
        Nothing -> do
          -- checkSuperClassForClassField (baseClass accClassType) pos ident
          let newCAttrs = M.insert ident t (cAttrs accClassType)
          return
            accClassType
              { cAttrs = newCAttrs
              }
    createClassTypeFromClassMembersHelp accClassType (A.ClassMethodT pos (A.FunDefT _ retType ident args _)) = do
      case M.lookup ident (cFuncs accClassType) of
        Just _ -> throwError $ functionAlreadyDeclaredForThisClass pos ident
        Nothing -> do
          let fType = A.TFun pos retType (P.map getArgType args)
          -- checkSuperClassForClassMethod (baseClass accClassType) pos ident fType
          let newCFuncs = M.insert ident fType (cFuncs accClassType)
          return
            accClassType
              { cFuncs = newCFuncs
              }

runStmtTEval :: Env -> StmtTEval a -> Either String (a, Env)
runStmtTEval env e = runIdentity (runExceptT (runStateT e env))

-- Build in functions
printString :: A.TopDef
printString =
  A.TopFuncDef noPos $
    A.FunDefT
      noPos
      (A.TVoid noPos)
      (A.UIdent "printString")
      [A.ArgT noPos (A.TStr noPos) (A.UIdent "x")]
      (A.SBlock noPos [])

printInt :: A.TopDef
printInt =
  A.TopFuncDef noPos $
    A.FunDefT
      noPos
      (A.TVoid noPos)
      (A.UIdent "printInt")
      [A.ArgT noPos (A.TInt noPos) (A.UIdent "x")]
      (A.SBlock noPos [])

addFunctions :: Env -> Env
addFunctions e = snd $ DE.fromRight ((), initEnv) $ runStmtTEval e x
  where
    x = do
      addTopDefToEnv SemanticAnalysis.printString
      addTopDefToEnv printInt

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
