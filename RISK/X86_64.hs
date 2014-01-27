module RISK.X86_64
  ( Reg (..)
  , X86
  , Instruction (..)
  , Label
  , AsmProgram
  , Asm (..)
  , codeGen
  , assemble
  -- * Assembly Statements
  , asm
  , label
  , comment
  , cCode
  -- * Instructions
  , callq
  , movq
  , popq
  , pushq
  , ret
  , xorl
  , jmp
  -- * Registers
  , rbp
  , rsp
  , rip
  , rdi
  , eax
  ) where

import MonadLib hiding (Label)
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

type X86 = StateT AsmProgram Id

-- | Assembles an X86 program.
assemble :: X86 () -> AsmProgram 
assemble = snd . runId . runStateT []

rbp = Rbp
rsp = Rsp
rip = Rip
rdi = Rdi
eax = Eax

type Label = String

type AsmProgram = [Asm]

data Asm
  = Label   Label
  | Instr   Instruction
  | Comment String
  | Custom  String
  | C       String

data Instruction
  = Callq Label
  | Movq  Reg Reg
  | Popq  Reg
  | Pushq Reg
  | Ret
  | Xorl  Reg Reg
  | Jmp   Label

asm :: Asm -> X86 ()
asm a = do
  p <- get
  set $ p ++ [a]

label :: String -> X86 ()
label = asm . Label

cCode :: Label -> X86 ()
cCode = asm . C

comment :: String -> X86 ()
comment = asm . Comment

instr :: Instruction -> X86 ()
instr = asm . Instr

callq :: Label -> X86 ()
callq a = instr $ Callq a

jmp :: Label -> X86 ()
jmp a = instr $ Jmp a

movq :: Reg -> Reg -> X86 ()
movq a b = instr $ Movq a b

popq :: Reg -> X86 ()
popq a = instr $ Popq a

pushq :: Reg -> X86 ()
pushq a = instr $ Pushq a

ret :: X86 ()
ret = instr $ Ret

xorl :: Reg -> Reg -> X86 ()
xorl a b = instr $ Xorl a b

codeGen :: Bool -> AsmProgram -> String
codeGen c = unlines . map codeGenAsm
  where
  
  asm :: String -> String
  asm a = if c then "asm (" ++ show a ++ ");" else a

  codeGenAsm :: Asm -> String
  codeGenAsm a = case a of
    Label a   -> asm $ printf "%s:" a
    Instr a   -> asm $ codeGenInstr a
    Comment a -> asm $ printf "# %s" a
    Custom  a -> asm $ a
    C       a -> a

codeGenInstr :: Instruction -> String
codeGenInstr a = case a of
  Callq a   -> printf "  callq %s" a
  Movq  a b -> printf "  movq  %%%s, %%%s" (show a) (show b)
  Popq  a   -> printf "  popq  %%%s" (show a)
  Pushq a   -> printf "  pushq %%%s" (show a)
  Ret       -> printf "  ret"
  Xorl  a b -> printf "  xorl  %%%s, %%%s" (show a) (show b)
  Jmp   a   -> printf "  jmp   %s" a

