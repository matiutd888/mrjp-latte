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

-- Make CustomData an instance of Semigroup
instance Semigroup X86Code where
  (<>) (X86Code x) (X86Code y) = mappend (X86Code x) (X86Code y)

-- Make CustomData an instance of Monoid
instance Monoid X86Code where
  -- mempty represents the empty value for your type
  mempty :: X86Code
  mempty = X86Code $ DList.fromList []

-- Intel syntax
data Asm = Return | Label String | Push Register | Mov Register Register | Sub Register Register | Pop Register

type Register = String

bytesOfInt :: Int
bytesOfInt = 4

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

instrToCode :: [Asm] -> X86Code
instrToCode x =
  X86Code
    { codeLines = DList.fromList x
    }

returnAddressOffset :: Int
returnAddressOffset = 4

sizeOfTypeBytes :: A.Type -> Int
sizeOfTypeBytes (A.TBool _) = 1
sizeOfTypeBytes _ = 4

registersStandard :: [Register]
registersStandard = ["eax", "edx", "ebx", "ecx", "esi", "edi"]

stackRegister :: Register
stackRegister = "esp"

frameRegister :: Register
frameRegister = "ebp"

registersAll :: [Register]
registersAll = stackRegister : frameRegister : registersStandard
