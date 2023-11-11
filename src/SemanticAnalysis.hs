{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalysis where

-- import qualified AbsLatte as A
-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import qualified Data.Set as S
-- import Errors
-- import System.IO
-- import Utils

-- data Env = Env
--   { variables :: S.Set A.Ident
--   }
--   deriving (Show)

-- type EvalT a = ReaderT Env (ExceptT String IO) a

-- initEnv :: Env
-- initEnv = Env S.empty

-- evalHelper :: A.Exp -> A.Exp -> EvalT ()
-- evalHelper e1 e2 = do
--   _ <- evalExpr e1
--   _ <- evalExpr e2
--   return ()

-- evalExpr :: A.Exp -> EvalT ()
-- evalExpr (A.ExpLit _ _) = return ()
-- evalExpr (A.ExpVar pos ident) = do
--   env <- ask
--   case S.member ident (variables env) of
--     False -> throwError $ semanticAnalysisError ++ (undefinedReferenceMessage ident pos)
--     True -> return ()
-- evalExpr (A.ExpAdd _ e1 e2) = evalHelper e1 e2
-- evalExpr (A.ExpMul _ e1 e2) = evalHelper e1 e2
-- -- We don't check division by zero
-- evalExpr (A.ExpDiv _ e1 e2) = evalHelper e1 e2
-- evalExpr (A.ExpSub _ e1 e2) = evalHelper e1 e2

-- evalStmt :: A.Stmt -> EvalT Env
-- evalStmt (A.SAss _ ident expr) = do
--   _ <- evalExpr expr
--   env <- ask
--   let vars = variables env
--   return $ Env {variables = S.insert ident vars}
-- evalStmt (A.SExp _ expr) = evalExpr expr >> ask

-- evalProgram :: A.Program -> EvalT ()
-- evalProgram (A.Prog _ stmts) = ask >>= \env -> composeReaders env (map evalStmt stmts) >> return ()

-- runEvalT :: Env -> EvalT a -> IO (Either String a)
-- runEvalT env e = runExceptT (runReaderT e env)

-- semanticAnalysis :: A.Program -> IO (Either String ())
-- semanticAnalysis p = runEvalT initEnv (evalProgram p)
