module RISK.X86_64
  ( Reg (..)
  , rbp
  , rsp
  , rip
  , rdi
  , eax
  , Instruction (..)
  , Label
  , Program
  , Asm (..)
  , codeGen
  ) where

import Text.Printf

data Reg
  = Rbp
  | Rsp
  | Rip
  | Rdi
  | Eax

instance Show Reg where
  show a = case a of
    Rbp -> "rbp"
    Rsp -> "rsp"
    Rip -> "rip"
    Rdi -> "rdi"
    Eax -> "eax"

rbp = Rbp
rsp = Rsp
rip = Rip
rdi = Rdi
eax = Eax

data Instruction
  = Callq Label
  | Movq  Reg Reg
  | Popq  Reg
  | Pushq Reg
  | Ret
  | Xorl  Reg Reg

type Label = String

type Program = [Asm]

data Asm
  = Label   Label
  | Instr   Instruction
  | Comment String
  | Custom  String

codeGen :: Program -> String
codeGen = unlines . map codeGenAsm

codeGenAsm :: Asm -> String
codeGenAsm a = case a of
  Label a -> printf "%s:" a
  Instr a -> codeGenInstr a
  Comment a -> printf "# %s" a
  Custom  a -> a

codeGenInstr :: Instruction -> String
codeGenInstr a = case a of
  Callq a   -> printf "  callq %s" a
  Movq  a b -> printf "  movq  %%%s, %%%s" (show a) (show b)
  Popq  a   -> printf "  popq  %%%s" (show a)
  Pushq a   -> printf "  pushq %%%s" (show a)
  Ret       -> printf "  ret"
  Xorl  a b -> printf "  xorl  %%%s, %%%s" (show a) (show b)

