module Main where

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

import Json
import Maybe
import String

echo : IO ()
echo = forever (getLine >>= putStrLn)

loop : IO ()
loop = getLine >>= \s ->
       if s == "exit"
       then pure ()
       else putStrLn s >> loop
       
hello : IO ()
hello = putStrLn "Hello, Console!"       >>
        putStrLn "I'll echo your input until you say \"exit\":" >>
        loop >>         
        putStrLn "That's all, folks! Here's some blahs:"  >>
        putStrLn (String.concat <| repeat 100000 "blah ") >>
        exit 0

-- | Can't use a type alias in ports, yet :/
port requests : Signal Request
port requests = Run.run responses hello

port responses : Signal Response
