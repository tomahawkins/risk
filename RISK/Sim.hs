-- | C simulation of a RISK kernel.
module RISK.Sim
  ( generateSimulator
  ) where

import Language.GIGL
import Text.Printf

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
  , "#include \"risk_lib.h\""
  , ""
  , "// Partition entry points."
  , unlines [ printf "void %s_main (void);" name | name <- partitionNames config ]
  , "// Partition memories (recv buffers + send buffers + data space)."
  , unlines [ printf "static word %s_memory[%d];" name $ partitionMemorySize config name | name <- partitionNames config ]
  , "// Variables from GIGL model."
  , unlines [ printf "static word %s;" name | name <- variables program ]
  , "// Set the partition memory pointers."
  , "void risk_set_memory_ptrs(void)"
  , "{"
  , unlines [ printf "\t%s_memory_ptr = (word) %s_memory;" name name | name <- partitionNames config ]
  , "}"
  , ""
  , compile spec
  , ""
  , "// RISK simulator main."
  , "int main (int argc, char **argv)"
  , "{"
  , "\trisk_cycle_count = atoi(argv[1]);"
  , "\trisk_init();"
  , "\treturn 0;"
  , "}"
  ]
  where
  config = configure spec
  program = kernelProgram spec

