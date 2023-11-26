{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text.Lazy.Builder
import Grammar.AbsLatte as A
import System.Exit (exitFailure)
import System.FilePath
import System.IO (IOMode (WriteMode), hPutStr, withFile)

writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile filePath content = do
  withFile filePath WriteMode $ \handle -> do
    hPutStr handle content

validateFilePath :: FilePath -> IO ()
-- validateFilePath f = if takeExtension f == ".input" then return () else hPutStrLn stderr "Unexpected extension of the input file; it should be .input" >> exitFailure
validateFilePath _ = return ()

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

maybeToError :: MonadError String m => Maybe a -> String -> m a
maybeToError (Just x) _ = return x
maybeToError Nothing s = throwError s

typesEq :: A.Type -> A.Type -> Bool
typesEq (A.TInt _) (A.TInt _) = True
typesEq (A.TStr _) (A.TStr _) = True
typesEq (A.TBool _) (A.TBool _) = True
typesEq (A.TVoid _) (A.TVoid _) = True
-- typesEq (A.NoType _) (A.NoType _) = True
typesEq (A.TFun _ ret1 args1) (A.TFun _ ret2 args2) =
  typesEq ret1 ret2 && and (zipWith typesEq args1 args2)
typesEq (A.TClass _ (A.UIdent x)) (A.TClass _ (A.UIdent y)) = x == y
typesEq _ _ = False

noPos :: A.BNFC'Position
noPos = A.BNFC'NoPosition

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

readInt :: A.TopDef
readInt =
  A.TopFuncDef noPos $
    A.FunDefT
      noPos
      (A.TInt noPos)
      (A.UIdent "readInt")
      []
      (A.SBlock noPos [A.SRet noPos (A.ELitInt noPos 0)])

readString :: A.TopDef
readString =
  A.TopFuncDef noPos $
    A.FunDefT
      noPos
      (A.TStr noPos)
      (A.UIdent "readString")
      []
      (A.SBlock noPos [A.SRet noPos (A.EString noPos "")])

builtInFunctions :: [TopDef]
builtInFunctions = [printInt, printString, readInt, readString]
