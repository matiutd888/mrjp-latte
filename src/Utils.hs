module Utils where

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
validateFilePath f = if takeExtension f == ".input" then return () else hPutStrLn stderr "Unexpected extension of the input file; it should be .input" >> exitFailure

composeReaders :: (MonadReader b m) => b -> [m b] -> m b
composeReaders b [] = return b
composeReaders _ [x] = x
composeReaders _ (x : xs) = x >>= \result -> local (const result) (composeReaders result xs)

-- To get maximum performance when building lazy Text values using a builder, associate mappend calls to the right.
appendInstruction :: String -> Builder -> Builder
appendInstruction s b = fromString (reverse (s ++ "\n")) <> b
