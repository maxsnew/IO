module Main where

import open IO.IO

console : IO ()
console = putStrLn "Hello, Console!"       >>
          putStrLn "I'll echo your input:" >>
          (getLine >>= putStrLn)           >>
          putStrLn "That's all, folks!"    >>
          exit 0