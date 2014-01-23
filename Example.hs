module Main (main) where

import RISK

main :: IO ()
main = do
  putStrLn  "Writing graphviz diagram of partitions specification (example.dot) ..."
  writeFile "example.dot" $ graphviz exampleSpec

  putStrLn  "Verifying kernel given partitions specification ..."
  verifyKernel exampleSpec

  putStrLn  "Generating API files for configured kernel ..."
  generateAPI exampleSpec

  putStrLn  "Generating C simulator of kernel for example configuration ..."
  generateSimulator exampleSpec

  -- Other things that you can do ...

  --putStrLn  "Generating example assembly program (hello.s) ..."
  --writeFile "hello.s" $ codeGen exampleAsm

  --putStrLn  "Kernel configuration from example specification:"
  --print     $ configure exampleSpec

  --putStrLn  "Kernel program of example configuration:"
  --print     $ kernelProgram exampleSpec

