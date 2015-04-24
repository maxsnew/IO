module Main where

import IO.IO exposing (..)

console : IO ()
console = putStrLn "Hello, Console!"       >>>
          putStrLn "I'll echo your input:" >>>
          (getLine >>= putStrLn)           >>>
          putStrLn "That's all, folks!"    >>>
          exit 0