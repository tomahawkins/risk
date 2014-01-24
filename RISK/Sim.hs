-- | C simulation of a RISK kernel.
module RISK.Sim
  ( generateSimulator
  ) where

import Text.Printf

import RISK.API (byte, word)
import RISK.Config
import RISK.Spec

generateSimulator :: Spec -> IO ()
generateSimulator spec = writeFile "risk_sim.c" $ unlines
  [ printf "// RISK Simulator"
  , printf ""
  , printf "#include <stdlib.h>"
  , printf "#include <stdio.h>"
  , printf ""
  , printf "// Partition entry points."
  , unlines [ printf "void %s_main (void);" name | name <- partitionNames config ]
  , printf "// Parition initialization flags."
  , unlines [ printf "static unsigned char %s_initialized;" name | name <- partitionNames config ]
  , printf "// Partition memories (recv buffers + send buffers + data space)."
  , unlines [ printf "static %s %s_memory[%d];" byte name $ partitionMemorySize config name | name <- partitionNames config ]
  , printf "// Partition channel buffers and data memory."
  , unlines $ concatMap partitionBuffers $ partitionMemory config
  , printf "// Partition stack pointers."
  , unlines [ printf "static void * %s_sp;" name | name <- partitionNames config ]
  , printf "// Active partition."
  , printf "static %s risk_active_partition;" word
  , printf ""
  , printf "// Current scheduling phase."
  , printf "static %s risk_scheduling_phase;" word
  , printf ""
  , printf "// Scheduling cycle count."
  , printf "static int risk_cycle_count;"
  , printf ""
  , printf "// Kernel initialization."
  , printf "void risk_init (void)"
  , printf "{"
  , indent $ unlines
    [ printf "// Initialize active partition and scheduling phase."
    , printf "risk_active_partition = 0xffffffffffffffff;"
    , printf "risk_scheduling_phase = %d;" $ length (schedule config) - 1
    , printf ""
    , printf "// Initialize partition init flags."
    , unlines [ printf "%s_initialized = 0;" name | name <- partitionNames config ]
    , printf "// Initialize partition stack pointers."
    , unlines [ printf "%s_sp = %s_memory + %d;" name name $ partitionMemorySize config name | name <- partitionNames config ]
    ]
  , printf "}"
  , printf ""
  , printf "// Kernel entry point."
  , printf "void risk_entry (void)"
  , printf "{"
  , indent $ unlines
    --XXX Why does this cause a Bus error: 10?
    --[ printf "printf(\"risk_yield: %%d\\n\", risk_active_partition);"
    --, printf "fflush(stdout);"
    --, printf ""
    [ printf "if (risk_cycle_count-- == 0) {"
    , printf "\texit(0);"
    , printf "}"
    , printf ""
    , printf "// Save the active partition's stack pointer."
    , printf "switch (risk_active_partition) {"
    , indent $ unlines [ printf "case %-3d : asm(\"movq %%%%rsp, %%0\" : \"=r\" (%s_sp) : ); break;" (partitionId config name) name | name <- partitionNames config ]
    , printf "}"
    , printf ""
    , printf "// Run the next partition."
    , printf "switch (risk_scheduling_phase) {"
    , indent $ unlines $ map scheduleCase sched
    , printf "}"
    , printf "asm(\"return_risk_entry:\");"
    ]
  , printf "}"
  , printf ""
  , printf "// Partition yields control back to the kernel."
  , printf "void risk_yield (void)"
  , printf "{"
  , printf "\trisk_entry();"
  , printf "}"
  , printf ""
  , printf "// Kernel's main."
  , printf "int main (int argc, char **argv)"
  , printf "{"
  , printf "\trisk_cycle_count = atoi(argv[1]);"
  , printf "\trisk_init();"
  , printf "\trisk_entry();"
  , printf "\treturn 0;"
  , printf "}"
  ]
  where
  config = configure spec
  sched :: [(Int, Int)]
  sched = zip s $ tail s ++ [head s]
    where
    s = [0 .. length (schedule config) - 1]

  scheduleCase :: (Int, Int) -> String
  scheduleCase (curr, next) = unlines
    [ printf "case %-3d :" curr
    , indent $ unlines
      [ printf "// Set new scheduling phase and active partition."
      , printf "risk_scheduling_phase = %d;" next
      , printf "risk_active_partition = %d;" $ partitionId config $ schedule config !! next
      , printf "// Load the partition's stack pointer."
      , printf "asm(\"movq %%0, %%%%rsp\" : : \"r\" (%s_sp));" name
      , printf "if (%s_initialized) {" name
      , indent $ unlines
        [ printf "// If partition is already running, return from its previous call to risk_yield."
        , printf "asm(\"addq $0x10,%%rsp\");"
        , printf "asm(\"popq %%rbp\");"
        , printf "asm(\"ret\");"
        ]
      , printf "}"
      , printf "else {"
      , indent $ unlines
        [ printf "// Else, initialize the partition by calling the partition's main entry point."
        , printf "%s_initialized = 1;" name
        , printf "%s_main();" name
        ]
      , printf "}"
      , printf "break;"
      ]
    ]
    where
    name = partitionNames config !! next

  indent = unlines . map ("\t" ++) . lines
  partitionBuffers :: (Name, PartitionMemory) -> [String]
  partitionBuffers (name, PartitionMemory recv' send' _) = recvPtrs 0 recv'
    where
    recvPtrs :: Integer -> [(Integer, Name)] -> [String]
    recvPtrs index a = case a of
      [] -> recv index recv'
      (_, from) : rest ->
        [ printf "%s * const %s_from_%s_head_index = (%s *) (%s_memory + %d);" word name from word name $ index
        , printf "%s * const %s_from_%s_tail_index = (%s *) (%s_memory + %d);" word name from word name $ index + 8
        ] ++ recvPtrs (index + 16) rest
    recv :: Integer -> [(Integer, Name)] -> [String]
    recv index a = case a of
      [] -> send index send'
      (s, from) : rest -> printf "%s * const %s_from_%s_recv_buffer = %s_memory + %d;" byte name from name index : recv (index + s) rest
    send :: Integer -> [(Integer, Name)] -> [String]
    send index a = case a of
      [] -> [printf "%s * const %s_data = %s_memory + %d;" byte name name index]
      (s, to) : rest -> printf "%s * const %s_to_%s_send_buffer = %s_memory + %d;" byte name to name index : send (index + s) rest

