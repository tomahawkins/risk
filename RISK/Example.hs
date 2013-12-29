-- | A RISK configuration example.
module RISK.Example
  ( exampleConfig
  , exampleAsm
  ) where

import RISK.Config
import RISK.X86_64

-- | A RISK configuration example.
exampleConfig :: Config
exampleConfig = Config
  { partitions =
    [ Partition { pName = "sensor_1",       pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_2",       pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_3",       pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_voting",  pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "control_law_1",  pRate =  500,  pMemory = 1024 }
    , Partition { pName = "control_law_2",  pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "actuator_1",     pRate =  500,  pMemory = 1024 }
    , Partition { pName = "actuator_2",     pRate =  500,  pMemory = 1024 }
    , Partition { pName = "actuator_3",     pRate = 1000,  pMemory = 1024 }
    ]
  , channels =
    [ ("sensor_1",       "sensor_voting",     MessagePassingAllowBackChannel 64)
    , ("sensor_2",       "sensor_voting",     MessagePassingAllowBackChannel 64)
    , ("sensor_3",       "sensor_voting",     MessagePassingAllowBackChannel 64)
    , ("sensor_voting",  "control_law_1",     MessagePassing 64)
    , ("sensor_voting",  "control_law_2",     MessagePassing 64)
    , ("control_law_1",  "actuator_1",        MessagePassing 64)
    , ("control_law_1",  "actuator_2",        MessagePassing 64)
    , ("control_law_2",  "actuator_3",        MessagePassing 64)
    ]
  , scheduling = []
  }

exampleAsm :: Program
exampleAsm =
  [ Custom ".private_extern _main"
  , Custom ".globl _main"
  , Label  "_main"
  , Instr  $ Pushq rbp
  , Instr  $ Movq  rsp rbp
  , Custom "  leaq  _helloMessage(%rip), %rdi"
  , Instr  $ Callq "_puts"
  , Instr  $ Xorl  eax eax
  , Instr  $ Popq  rbp
  , Instr  $ Ret
  , Custom ".section __TEXT,__cstring,cstring_literals"
  , Label  "_helloMessage"
  , Custom "  .asciz \"Hello World!\""
  ]

