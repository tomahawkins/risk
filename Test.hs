module Main (main) where

import RISK

main :: IO ()
main = putStrLn $ graphviz exampleConfig

