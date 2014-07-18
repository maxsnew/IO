module Main where

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

import Json
import String

hugeString () = String.concat <| repeat 100000000000 "blah "

port requests : Signal Json.Value
port requests = Run.run responses (putStrLn "hah" >>= \_ -> putStrLn (hugeString ()))

port responses : Signal (Maybe String)
