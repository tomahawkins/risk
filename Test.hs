module Main (main) where

import RISK

main :: IO ()
main = do
  writeFile "test.dot" $ graphviz exampleConfig
  writeFile "hello.s"  $ codeGen  exampleAsm


