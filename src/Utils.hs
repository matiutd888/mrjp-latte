{-# LANGUAGE FlexibleContexts #-}

module Utils where

import AbsLatte as A
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text.Lazy.Builder
import System.Exit (exitFailure)
import System.FilePath
import System.IO

writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile filePath content = do
  withFile filePath WriteMode $ \handle -> do
    hPutStr handle content

validateFilePath :: FilePath -> IO ()
-- validateFilePath f = if takeExtension f == ".input" then return () else hPutStrLn stderr "Unexpected extension of the input file; it should be .input" >> exitFailure
validateFilePath f = return ()

composeReaders :: (MonadReader b m) => b -> [m b] -> m b
composeReaders b [] = return b
composeReaders _ [x] = x
composeReaders _ (x : xs) = x >>= \result -> local (const result) (composeReaders result xs)

-- To get maximum performance when building lazy Text values using a builder, associate mappend calls to the right.
appendInstruction :: String -> Builder -> Builder
appendInstruction s b = fromString (reverse (s ++ "\n")) <> b

assertM :: MonadError String m => Bool -> String -> m ()
assertM b s =
  if b
    then return ()
    else throwError s

isType :: A.Type -> (BNFC'Position -> A.Type) -> Bool
isType t1 t2 = typesEq t1 $ t2 BNFC'NoPosition

typesEq :: A.Type -> A.Type -> Bool
typesEq (A.TInt _) (A.TInt _) = True
typesEq (A.TStr _) (A.TStr _) = True
typesEq (A.TBool _) (A.TBool _) = True
typesEq (A.TVoid _) (A.TVoid _) = True
-- typesEq (A.NoType _) (A.NoType _) = True
typesEq (A.TFun _ ret1 args1) (A.TFun _ ret2 args2) =
  typesEq ret1 ret2 && and (Prelude.zipWith typesEq args1 args2)
typesEq (A.TClass _ (A.UIdent x)) (A.TClass _ (A.UIdent y)) = x == y
typesEq _ _ = False

noPos :: A.BNFC'Position
noPos = A.BNFC'NoPosition
