module Main where

import IO.IO exposing (..)
import IO.Runner as IO

import List
import Maybe
import String
import Task

echo : IO ()
echo = forever (getLine >>= putStrLn)

loop : IO ()
loop = getLine >>= \s ->
       if s == "exit"
       then pure ()
       else putStrLn s >>> loop
       
hello : IO ()
hello = putStrLn "Hello, Console!" >>>
        writeFile { file = "test.txt", content = "hellow file" } >>>
        putStrLn "I'll echo your input until you say \"exit\":" >>>
        loop >>>
        putStrLn "That's all, folks! Here's some blahs:"  >>>
        putStrLn (String.concat <| List.repeat 100000 "blah ") >>>
        exit 0

port runner : Signal (Task.Task x ())
port runner = IO.run hello
