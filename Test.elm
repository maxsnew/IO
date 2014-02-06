module Main where

import open IO.IO
import IO.Runner (Request, Response)
import IO.Runner as Run

import String

echo : IO ()
echo = forever (getLine >>= putStrLn)

hello : IO ()
hello = putStrLn "Hello, Console!" >>
        putStrLn "I'll echo your input:" >>
        (getLine >>= putStrLn) >>
        putStrLn "That's all, folks!" >>
        exit 0

port requests : Signal { mPut  : Maybe String
                       , mExit : Maybe Int
                       , mGet  : Bool
                       }
port requests = Run.run responses hello

port responses : Signal (Maybe String)
