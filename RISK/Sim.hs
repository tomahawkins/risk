-- | C simulation of a RISK kernel.
module RISK.Sim
  ( generateSimulator
  ) where

import Text.Printf

import RISK.API (byte)
import RISK.Config
import RISK.Spec

generateSimulator :: Spec -> IO ()
generateSimulator spec = writeFile "risk_sim.c" $ unlines
  [ printf "// RISK Kernel Simulator"
  , printf ""
  , printf "#include <stdio.h>"
  , printf ""
  , printf "// Partition entry points."
  , unlines [ printf "void %s_main (void);" name | name <- partitionNames config ]
  , printf "// Partition memories (recv buffers + send buffers + data space)."
  , unlines [ printf "static %s %s_memory[%d];" byte name $ partitionMemorySize config name | name <- partitionNames config ]
  , printf "// Partition stack pointers."
  , unlines [ printf "static void * %s_sp;" name | name <- partitionNames config ]
  , printf "// Kernel stack pointer."
  , printf "static void * risk_sp;"
  , printf ""
  , printf "// Active partition."
  , printf "static int risk_active_partition;"
  , printf ""
  , printf "// Partition yields control back to the kernel."
  , printf "void risk_yield (void)"
  , printf "{"
  , indent $ unlines
    --XXX Why does this cause a Bus error: 10?
    --[ printf "printf(\"risk_yield: %%d\\n\", risk_active_partition);"
    --, printf "fflush(stdout);"
    --, printf ""
    [ printf "// Save the partition's stack pointer."
    , printf "switch (risk_active_partition) {"
    , indent $ unlines [ printf "case %-3d : asm(\"movq %%%%rsp, %%0\" : \"=r\" (%s_sp) : ); break;" (partitionId config name) name | name <- partitionNames config ]
    , printf "}"
    , printf ""
    , printf "// Restore the kernel's stack pointer."
    , printf "asm(\"movq %%0, %%%%rsp\" : : \"r\" (risk_sp));"
    , printf ""
    , printf "// Return from previous call to risk_run_partition."
    , printf "asm(\"addq $0x20,%%rsp\");"
    , printf "asm(\"popq %%rbp\");"
    , printf "asm(\"ret\");"
    , printf "asm(\"return_risk_yield:\");"
    ]
  , printf "}"
  , printf ""
  , printf "// Runs the next partition in the schedule."
  , printf "static void risk_run_partition(int partition, unsigned char * partition_initialized, void * partition_sp, void (* partition_main)(void))"
  , printf "{"
  , indent $ unlines
    [ printf "printf(\"risk_run_partition: %%d\\n\", partition);"
    , printf "fflush(stdout);"
    , printf ""
    , printf "// Set the active partition."
    , printf "risk_active_partition = partition;"
    , printf ""
    , printf "// Save the return address on the kernel stack (for a ret in the yield function)."
    , printf "//asm(\"push %%l0\" : : : return_from_risk_run_partition);"
    , printf ""
    , printf "// Save the kernel's stack pointer."
    , printf "asm(\"movq %%%%rsp, %%0\" : \"=r\" (risk_sp) : );"
    , printf ""
    , printf "// Load the partition's stack pointer."
    , printf "asm(\"movq %%0, %%%%rsp\" : : \"r\" (partition_sp));"
    , printf ""
    , printf "if (* partition_initialized) {" 
    , printf "\t// If partition is already running, return from its previous call to risk_yield."
    --, printf "\tasm(\"addq $0x10,%%rsp\");"
    , printf "\tasm(\"popq %%rbp\");"
    , printf "\tasm(\"ret\");"
    , printf "}"
    , printf "else {"
    , printf "\t// Else, initialize the partition by calling the partition's main entry point."
    , printf "\t* partition_initialized = 1;"
    , printf "\tpartition_main();"
    , printf "}"
    , printf "asm(\"return_risk_run_partition:\");"
    ]
  , printf "}"
  , printf ""
  , printf "// Kernel main entry point."
  , printf "int main (void)"
  , printf "{"
  , indent $ unlines
    [ printf "// Partition initalization flags."
    , unlines [ printf "unsigned char %s_initialized = 0;" name | name <- partitionNames config ]
    , printf "// Initialize partition stack pointers."
    , unlines [ printf "%s_sp = %s_memory + %d;" name name $ partitionMemorySize config name | name <- partitionNames config ]
    , printf "// Run the partition scheduler."
    , printf "for (;;) {"
    , indent $ unlines [ printf "risk_run_partition(%d, & %s_initialized, %s_sp, %s_main);" (partitionId config name) name name name | name <- schedule config ]
    , printf "}"
    , printf "return 0;"
    ]
  , printf "}"
  ]
  where
  config = configure spec
  indent = unlines . map ("\t" ++) . lines
