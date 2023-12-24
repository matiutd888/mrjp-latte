{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module UtilsX86 where

import Control.Exception
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text.Lazy.Builder
import qualified Grammar.AbsLatte as A

data X86Code = X86Code
  { codeLines :: DList.DList Asm
  }

instance Semigroup X86Code where
  (<>) :: X86Code -> X86Code -> X86Code
  (<>) (X86Code x) (X86Code y) = mappend (X86Code x) (X86Code y)

instance Monoid X86Code where
  mempty :: X86Code
  mempty = X86Code $ DList.fromList []

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
  | Sar Integer
  | Jne String
  | Ja String
  | Jb String
  | Jae String
  | Jbe String
  | Lea Operand Operand

type Register = String

data Operand = Reg Register | SimpleMem Register Int | Constant Int | StringConstant String

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

instrToString :: Asm -> String
instrToString = undefined

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
