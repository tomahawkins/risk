-- | C simulation of a RISK kernel.
module RISK.Sim
  ( generateSimulator
  ) where

import Language.GIGL
import Text.Printf

import RISK.API (byte, word)
import RISK.Compile
import RISK.Config
import RISK.Kernel
import RISK.Spec

generateSimulator :: Spec -> IO ()
generateSimulator spec = writeFile "risk_sim.c" $ unlines
  [ "// RISK Simulator"
  , ""
  , "#include <stdlib.h>"
  , "#include <stdio.h>"
  , ""
  , "// Scheduling cycle count."
  , "static int risk_cycle_count;"
  , ""
  , "// Partition entry points."
  , unlines [ printf "void %s_main (void);" name | name <- partitionNames config ]
  , "// Partition memories (recv buffers + send buffers + data space)."
  , unlines [ printf "static %s %s_memory[%d];" byte name $ partitionMemorySize config name | name <- partitionNames config ]
  , "// Variables from GIGL model."
  , unlines [ printf "static %s %s;" word name | name <- variables program ]
  , "// Set the partition memory pointers."
  , "void risk_set_memory_ptrs(void)"
  , "{"
  , unlines [ printf "\t%s_memory_ptr = (%s) %s_memory;" name word name | name <- partitionNames config ]
  , "}"
  , ""
  , "// Kernel entry point."
  , "static void risk_entry (void)"
  , "{"
  , indent $ unlines
    --XXX Why does a printf here with arguments here cause a Bus error: 10?
    --[ printf "printf(\"asdf %%d\\n\", 1);"
    [ printf "// Exit the simulator if the cycle count drops to 0."
    , printf "if (risk_cycle_count-- == 0)"
    , printf "\texit(0);"
    , printf ""
    , printf "// Save the active partition's stack pointer."
    , printf "switch (risk_active_partition) {"
    , indent $ unlines [ printf "case %-3d : asm(\"movq %%%%rsp, %%0\" : \"=r\" (%s_stack_ptr) : ); break;" (partitionId config name) name | name <- partitionNames config ]
    , printf "}"
    , printf ""
    , printf "// Run the next partition."
    , printf "switch (risk_scheduling_phase) {"
    , indent $ unlines $ map (scheduleCase config) sched
    , printf "}"
    ]
  , printf "}"
  , printf ""
  , printf "// Partition yields control back to the kernel."
  , printf "void risk_yield (void)"
  , printf "{"
  , printf "\trisk_entry();"
  , printf "}"
  , printf ""
  , "// GIGL generated procedures."
  , compile spec
  , printf ""
  , printf "// RISK simulator main."
  , printf "int main (int argc, char **argv)"
  , printf "{"
  , printf "\trisk_cycle_count = atoi(argv[1]);"
  , printf "\trisk_init();"
  , printf "\treturn 0;"
  , printf "}"
  ]
  where
  config = configure spec
  sched :: [(Int, Int)]
  sched = zip s $ tail s ++ [head s]
    where
    s = [0 .. length (schedule config) - 1]
  program = kernelProgram spec

scheduleCase :: Config -> (Int, Int) -> String
scheduleCase config (curr, next) = unlines
  [ printf "case %-3d :" curr
  , indent $ unlines
    [ printf "// Set new scheduling phase and active partition."
    , printf "risk_scheduling_phase = %d;" next
    , printf "risk_active_partition = %d;" $ partitionId config $ schedule config !! next
    , printf "// Load the partition's stack pointer."
    , printf "asm(\"movq %%0, %%%%rsp\" : : \"r\" (%s_stack_ptr));" name
    , printf "if (%s_initialized) {" name
    , indent $ unlines
      [ printf "// If partition is already running, return from its previous call to risk_yield."
      , printf "return;"
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

