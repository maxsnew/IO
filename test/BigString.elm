module Main where

import IO exposing (..)
import IO
import String
import Task

hugeString () = String.concat <| List.repeat 10000000 "blah "

port runner : Signal (Task.Task x ())
port runner = IO.run (putStrLn "hah" >>= \_ -> putStrLn (hugeString ()))
