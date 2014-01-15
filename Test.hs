module Main (main) where

import RISK

main :: IO ()
main = do
  putStrLn  "Writing graphviz diagram of kernel specification..."
  writeFile "test.dot" $ graphviz exampleSpec

  putStrLn  "Kernel configuration from example specification:"
  print     $ configure exampleSpec

  putStrLn  "Generating example assembly program..."
  writeFile "hello.s" $ codeGen exampleAsm


