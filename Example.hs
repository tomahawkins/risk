module Main (main) where

import RISK

main :: IO ()
main = do
  putStrLn  "Writing graphviz diagram of partitions specification (example.dot) ..."
  writeFile "example.dot" $ graphviz exampleSpec

  putStrLn  "Verifying kernel ..."
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
    [ Partition { pName = "sensor",   pRate = Nothing,  pMemory = 10 }
    , Partition { pName = "control",  pRate = Nothing,  pMemory = 10 }
    , Partition { pName = "actuator", pRate = Nothing,  pMemory = 10 }
    ]
  , channels =
    [ Channel "sensor"  3  "control"  3
    , Channel "control" 3  "actuator" 3
    ]
  , scheduling = []
  }

