module Main (main) where

import RISK

main :: IO ()
main = do
  putStrLn  "Writing graphviz diagram of partitions specification (example.dot) ..."
  writeFile "example.dot" $ graphviz exampleSpec

  putStrLn  "Verifying kernel given partitions specification ..."
  verifyKernel exampleSpec

  putStrLn  "Generating C simulator of kernel for example configuration ..."
  generateSimulator exampleSpec

  -- Other things you can do ...

  --putStrLn  "Generating example assembly program (hello.s) ..."
  --writeFile "hello.s" $ codeGen exampleAsm

  --putStrLn  "Kernel configuration from example specification:"
  --print     $ configure exampleSpec

  --putStrLn  "Kernel program of example configuration:"
  --print     $ kernelProgram exampleSpec


-- | An example RISK specification.
exampleSpec :: Spec
exampleSpec = Spec
  { partitions =
    [ Partition { pName = "sensor_1",       pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "sensor_2",       pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "sensor_3",       pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "sensor_voting",  pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "control_law_1",  pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "control_law_2",  pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "actuator_1",     pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "actuator_2",     pRate = Nothing,  pMemory = 1024 }
    , Partition { pName = "actuator_3",     pRate = Nothing,  pMemory = 1024 }
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

