module Main where

import IO.IO (..)
import IO.Runner as Run
import Text (plainText)

import List
import Maybe
import String

-- echo : IO ()
-- echo = forever (getLine >>= putStrLn)

-- loop : IO ()
-- loop = getLine >>= \s ->
--        if s == "exit"
--        then pure ()
--        else putStrLn s >>> loop
       
hello : IO ()
hello = putStrLn "Hello, Console!" >>>
        -- putStrLn "I'll echo your input until you say \"exit\":" >>>
        -- loop >>>
        putStrLn "That's all, folks! Here's some blahs:"  >>>
        putStrLn (String.concat <| List.repeat 10 "blah ") >>>
        exit 0

gulp = Run.run hello
