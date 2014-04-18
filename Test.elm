module Main where

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

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
        putStrLn "That's all, folks!"    >>
        exit 0

-- | Can't use a type alias in ports, yet :/
port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses hello

port responses : Signal (Maybe String)
