{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module UtilsX86 where

import Control.Exception
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.DList as DList.DList
import Data.Text.Lazy.Builder
import qualified Grammar.AbsLatte as A

data X86Code = X86Code
  { codeLines :: DList.DList Asm
  }
  deriving (Show)

instance Semigroup X86Code where
  (<>) :: X86Code -> X86Code -> X86Code
  (<>) (X86Code x) (X86Code y) = X86Code (x `DList.append` y)

instance Monoid X86Code where
  mempty :: X86Code
  mempty = X86Code $ DList.fromList []

type Register = String

data Operand = Reg Register | SimpleMem Register Int | Constant Int | StringConstant String deriving (Show)

-- _registersOriginal :: [[Char]]
-- _registersOriginal = ["rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi"]

-- _registersAdded :: [[Char]]
-- _registersAdded = ["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

-- registers64 :: [Register]
-- registers64 = _registersOriginal ++ _registersAdded

-- change64To32 :: Register -> Register
-- change64To32 r = change64To32Help $ assert (head r == 'r') r
--   where
--     change64To32Help :: Register -> Register
--     change64To32Help r
--       | elem r _registersOriginal = 'e' : tail r
--       | elem r _registersAdded = tail r ++ "d"
--       | otherwise = undefined

--

operandToString :: Operand -> String
operandToString (Reg register) = "%" ++ register
operandToString (SimpleMem reg int) = show int ++ " (%" ++ reg ++ ")"
operandToString (Constant int) = "$" ++ show int
operandToString (StringConstant s) = "$" ++ show s

helperI2Str2Operands :: String -> Operand -> Operand -> String
helperI2Str2Operands s o1 o2 = indent ++ s ++ " " ++ operandToString o1 ++ ", " ++ operandToString o2

helperI2Str1Operand :: String -> Operand -> String
helperI2Str1Operand s o = indent ++ s ++ " " ++ operandToString o

helperI2Str1String :: String -> String -> String
helperI2Str1String s o = indent ++ s ++ " " ++ o

indent :: String
indent = "\t"

instrToString :: Asm -> String
instrToString Return = indent ++ "ret"
instrToString (Label s) = s ++ ":"
instrToString (Push s) = helperI2Str1Operand "push" s
instrToString (Mov dest src) = helperI2Str2Operands "mov" src dest
instrToString (Sub x y) = helperI2Str2Operands "sub" y x
instrToString (Add x y) = helperI2Str2Operands "add" y x
instrToString (Pop o) = helperI2Str1Operand "pop" o
instrToString (Cmp x y) = helperI2Str2Operands "cmp" y x
instrToString (Je x) = helperI2Str1String "je" x
instrToString (Jmp x) = helperI2Str1String "jmp" x
instrToString (Call x) = helperI2Str1String "call" x
instrToString (Neg x) = helperI2Str1Operand "neg" x
instrToString (Xor x y) = helperI2Str2Operands "xor" y x
instrToString (And x y) = helperI2Str2Operands "and" y x
instrToString (Or x y) = helperI2Str2Operands "or" y x
instrToString (Imul x y) = helperI2Str2Operands "imul" y x
instrToString (Idiv x) = helperI2Str1Operand "idiv" x
instrToString (Sar x y) = helperI2Str2Operands "sar" y x
instrToString (Jne x) = helperI2Str1String "jne" x
instrToString (Ja x) = helperI2Str1String "ja" x
instrToString (Jb x) = helperI2Str1String "jb" x
instrToString (Jae x) = helperI2Str1String "jae" x
instrToString (Jbe x) = helperI2Str1String "jbe" x
instrToString (Lea x y) = helperI2Str2Operands "lea" y x
instrToString Newline = "\n\n"
instrToString (StringConstantDeclaration label str) = label ++ ":\n" ++ "\t.string\t" ++ "\"" ++ str ++ "\""
instrToString GlobalHeader = ".global main"

-- Intel syntax
data Asm
  = Return
  | Label String
  | Push Operand
  | Mov Operand Operand
  | Sub Operand Operand
  | Add Operand Operand
  | Pop Operand
  | Cmp Operand Operand
  | Je String
  | Jmp String
  | Call String
  | Neg Operand
  | Xor Operand Operand
  | And Operand Operand
  | Or Operand Operand
  | Imul Operand Operand
  | Idiv Operand
  | Sar Operand Operand
  | Jne String
  | Ja String
  | Jb String
  | Jae String
  | Jbe String
  | Lea Operand Operand
  | Newline
  | StringConstantDeclaration String String
  | GlobalHeader
  deriving (Show)

popToNothing :: X86Code
popToNothing = instrsToCode [Add (Reg stackRegister) (Constant 4)]

instrToCode :: Asm -> X86Code
instrToCode = instrsToCode . (: [])

instrsToCode :: [Asm] -> X86Code
instrsToCode x =
  X86Code
    { codeLines = DList.fromList $ reverse x
    }

returnAddressOffset :: Int
returnAddressOffset = 4

-- TODO change size of bools to 1
sizeOfTypeBytes :: A.Type -> Int
sizeOfTypeBytes (A.TBool _) = 4
sizeOfTypeBytes _ = 4

registersStandard :: [Register]
registersStandard = ["eax", "edx", "ebx", "ecx", "esi", "edi"]

stackRegister :: Register
stackRegister = "esp"

frameRegister :: Register
frameRegister = "ebp"

resultRegister :: Register
resultRegister = "eax"

registersAll :: [Register]
registersAll = stackRegister : frameRegister : registersStandard

helperConcatStrings :: String
helperConcatStrings = "concat_strings"

helperStringsEqual :: String
helperStringsEqual = "compare_strings"
