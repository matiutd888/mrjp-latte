-- File generated by the BNF Converter (bnfc 2.9.4).

-- | Program to test parser.
module Main where

import qualified Compile as C
import Control.Monad
import Grammar.AbsLatte (Program)
import Grammar.LexLatte (Token, mkPosToken)
import Grammar.ParLatte (myLexer, pProgram)
import Grammar.PrintLatte (Print, printTree)
import Grammar.SkelLatte ()
import qualified SemanticAnalysis as SA
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Process
import Utils
import Prelude
  ( Either (..),
    IO,
    Int,
    Show,
    String,
    concat,
    putStrLn,
    readFile,
    show,
    unlines,
    ($),
    (++),
    (.),
    (>),
  )

type ParseFun a = [Token] -> Either String a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

getOutputFileFromInputFilePath :: FilePath -> FilePath
getOutputFileFromInputFilePath filePath = dropExtension filePath <.> ".s"

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = do
  validateFilePath f
  programStr <- readFile f
  let outputFilePath = getOutputFileFromInputFilePath f
  programCodeStr <- run v p programStr
  writeStringToFile outputFilePath programCodeStr
  createBinaryFromGASFile outputFilePath
  putStrLn f
  putStrLn programCodeStr
  return ()

runtimeObjectfile :: String
runtimeObjectfile = "lib/runtime.o"

createBinaryFromGASFile :: FilePath -> IO ()
createBinaryFromGASFile filePath = do
  let outputFile = dropExtension filePath
  let command = "gcc -m32 -o " ++ outputFile ++ " " ++ filePath ++ " " ++ runtimeObjectfile
  putStrLn command
  (_, _, _, processHandle) <- createProcess (shell command)
  exitCode <- waitForProcess processHandle
  case exitCode of
    ExitSuccess -> putStrLn $ "Executable file generated successfully: " ++ outputFile
    ExitFailure _ -> hPutStrLn stderr "gcc process encountered an error." >> exitFailure

run :: Verbosity -> ParseFun Program -> String -> IO String
run v p s =
  case p ts of
    Left err -> do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      -- putStrLn "\nParse Successful!"
      -- showTree v tree
      -- putStrLn "\n"
      getLLFileContent tree
  where
    ts = myLexer s
    showPosToken ((l, c), t) = concat [show l, ":", show c, "\t", show t]
    getLLFileContent :: Program -> IO String
    getLLFileContent prog = do
      let semAnalysisResult = SA.runSemanticAnalysis prog
      case semAnalysisResult of
        Left m -> hPutStrLn stderr "ERROR" >> hPutStrLn stderr m >> exitFailure
        Right (_, environment) -> do
          let f = SA.functions environment
          let c = SA.classes environment
          compilationResult <- C.runCompileProgram f c prog
          case compilationResult of
            Left m -> hPutStrLn stderr "COMPILING STAGE ERROR" >> hPutStrLn stderr m >> exitFailure
            Right (pStr, _) -> hPutStrLn stderr "OK" >> return pStr

-- llFileContent <- runExceptT $ getLLFile prog
-- case llFileContent of
--   Left m -> hPutStrLn stderr m >> exitFailure
--   Right m -> putStrLn m >> return m

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following arguments:",
        "  --help          Display this help message.",
        "  <file>          Run Latte compiler."
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [fs] -> runFile 2 pProgram fs >> return ()
    _ -> hPutStrLn stderr "Wrong program arguments (see --help)" >> exitFailure
