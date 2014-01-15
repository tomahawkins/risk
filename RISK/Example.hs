-- | An example RISK specification
module RISK.Example
  ( exampleSpec
  , exampleAsm
  ) where

import RISK.Spec
import RISK.X86_64

-- | An example RISK specification
exampleSpec :: Spec
exampleSpec = Spec
  { partitions =
    [ Partition { pName = "sensor_1",       pRate = Just 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_2",       pRate = Just 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_3",       pRate = Just 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_voting",  pRate = Just 1000,  pMemory = 1024 }
    , Partition { pName = "control_law_1",  pRate = Just  500,  pMemory = 1024 }
    , Partition { pName = "control_law_2",  pRate = Just 1000,  pMemory = 1024 }
    , Partition { pName = "actuator_1",     pRate = Just  500,  pMemory = 1024 }
    , Partition { pName = "actuator_2",     pRate = Just  500,  pMemory = 1024 }
    , Partition { pName = "actuator_3",     pRate = Just 1000,  pMemory = 1024 }
    ]
  , channels =
    [ Channel "sensor_1"      64  "sensor_voting" 64
    , Channel "sensor_2"      64  "sensor_voting" 64
    , Channel "sensor_3"      64  "sensor_voting" 64
    , Channel "sensor_voting" 64  "control_law_1" 64
    , Channel "sensor_voting" 64  "control_law_2" 64
    , Channel "control_law_1" 64  "actuator_1"    64
    , Channel "control_law_1" 64  "actuator_2"    64
    , Channel "control_law_2" 64  "actuator_2"    64
    , Channel "control_law_2" 64  "actuator_3"    64
    ]
  , scheduling = []
  }

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

