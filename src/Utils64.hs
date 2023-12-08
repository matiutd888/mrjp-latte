module Utils64 where

import Control.Exception

data Asm = Return | Label String | Push String | Mov

type Register = String

_registersOriginal :: [[Char]]
_registersOriginal = ["rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rsi", "rdi"]

_registersAdded :: [[Char]]
_registersAdded = ["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

registers64 :: [Register]
registers64 = _registersOriginal ++ _registersAdded

change64To32 :: Register -> Register
change64To32 r = change64To32Help $ assert (head r == 'r') r
  where
    change64To32Help :: Register -> Register
    change64To32Help r
      | elem r _registersOriginal = 'e' : tail r
      | elem r _registersAdded = tail r ++ "d"
      | otherwise = undefined
