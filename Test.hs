module Main (main) where

import RISK

main :: IO ()
main = do
  putStrLn  "Generating example assembly program (hello.s)..."
  writeFile "hello.s" $ codeGen exampleAsm

  putStrLn  "Writing graphviz diagram of kernel specification (test.dot)..."
  writeFile "test.dot" $ graphviz exampleSpec

  putStrLn  "Kernel configuration from example specification:"
  print     $ configure exampleSpec

  putStrLn  "Kernel program of example configuration:"
  print     $ kernelProgram exampleSpec

