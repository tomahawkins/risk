module RISK.Example
  ( exampleAsm
  ) where

import RISK.X86_64

exampleAsm :: Program
exampleAsm =
  [ Custom  ".private_extern _main"
  , Custom  ".globl _main"
  , Comment "Main entry point."
  , Label   "_main"
  , Instr   $ Pushq rbp
  , Instr   $ Movq  rsp rbp
  , Custom  "  leaq  _helloMessage(%rip), %rdi"
  , Instr   $ Callq "_puts"
  , Instr   $ Xorl  eax eax
  , Instr   $ Popq  rbp
  , Instr   $ Ret
  , Custom  ".section __TEXT,__cstring,cstring_literals"
  , Label   "_helloMessage"
  , Custom  "  .asciz \"Hello World!\""
  ]

