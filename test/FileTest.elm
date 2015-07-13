module Main where

import IO exposing (..)
import IO
import Task

console : IO ()
console = writeFile { file = "Test.txt", content = "Hello, Test!\n" } >>>
          putStrLn "Printed to file: Test.txt"

port runner : Signal (Task.Task x ())
port runner = run responses

